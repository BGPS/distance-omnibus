;+
; NAME:
;       OMNI_READ_VEL
;
; PURPOSE:
;       Wrapper for the READ_MRT routine for the purpose of reading in
;       a velocity MRT, taking into account the coordinate system used
;       in that file.  Returns a structure in a standardized format.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       vs = OMNI_READ_VEL( confstr, coordtype [,count] )
;
; INPUTS:
;       CONFSTR   -- Configuration structure conf.v
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       VS -- Structure containing the data in the MRT in standardized
;             format for general consumption.
;
; OPTIONAL OUTPUTS:
;       COUNT -- Number of elements (objects) in the structure.
;
; MODIFICATION HISTORY:
;
;       Created:  02/25/13, TPEB -- Initial version.
;       Modified: 03/12/13, TPEB -- Only return objects with FLAG = 1
;
;-

FUNCTION OMNI_READ_VEL, v, count
  
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Check that the mrt can be read
  IF ~FILE_TEST(v.mrt,/read) THEN BEGIN
     message,'Error: Velocity MRT file '+v.mrt+' does not exist!',/cont
     count = 0
     RETURN,{error:'ERROR: '+v.mrt+' does not exist'}
  ENDIF
  
  ;; Read in the survey catalog & count entries
  vs = read_mrt(v.mrt, count=count)
  
  IF TAG_EXIST(vs,'GLON') && TAG_EXIST(vs,'GLAT') THEN v.coord = 21
  
  CASE v.coord OF
     21: BEGIN                  ; Galactic coordinates
        IF ~TAG_EXIST(vs,'GLON') || ~TAG_EXIST(vs,'GLON') THEN BEGIN
           message,'Error: Galactic coordinates specified for '+v.mrt+$
                   ', but GLON and GLAT are not present',/cont
           count = 0
           RETURN,{error:'ERROR: Galactic coords specified, but no GLON, GLAT'}
        ENDIF
        l = vs.glon
        b = vs.glat
     END
     
     22: BEGIN             ; J2000 Equatorial coordinates
        ;; Check whether coordinates are in degrees or h:m:s
        IF TAG_EXIST(vs,'RAh') THEN BEGIN ; H:M:S
           ra = (vs.rah + vs.ram/60.d + vs.ras/3600.d) * 15.
           ds = TAG_EXIST(vs,'DE-') ? double(vs.de_+'1') : 1.d
           dec = ds*(vs.ded + vs.dem/60.d + vs.des/3600.d)
        ENDIF ELSE IF TAG_EXIST(vs,'RA') THEN BEGIN
           ra = vs.ra
           dec = vs.dec
        ENDIF ELSE BEGIN
           message,'Error: Equatorial coordinates specified for '+v.mrt+$
                   ', but RA or RAh are not present',/cont
           count = 0
           RETURN,{error:'ERROR: Equatorial coords specified, but no RA, RAh'}
        ENDELSE
        euler,ra,dec,l,b,1
     END
     
     23: BEGIN             ; B1950 Equatorial coordinates
        ;; Check whether coordinates are in degrees or h:m:s
        IF TAG_EXIST(vs,'RAh') THEN BEGIN ; H:M:S
           ra = (vs.rah + vs.ram/60.d + vs.ras/3600.d) * 15.
           ds = TAG_EXIST(vs,'DE-') ? double(vs.de_+'1') : 1.d
           dec = ds*(vs.ded + vs.dem/60.d + vs.des/3600.d)
        ENDIF ELSE IF TAG_EXIST(vs,'RA') THEN BEGIN
           ra = vs.ra
           dec = vs.dec
        ENDIF ELSE BEGIN
           message,'Error: Equatorial coordinates specified for '+v.mrt+$
                   ', but RA or RAh are not present',/cont
           count = 0
           RETURN,{error:'ERROR: Equatorial coords specified, but no RA, RAh'}
        ENDELSE
        euler,ra,dec,l,b,1,/FK4
     END
     
     ELSE: BEGIN
        message,'Error: Unknown coordinate value '+$
                string(v.coord,format="(I0)"),/cont
        count = 0
        RETURN,{error:'ERROR: Unrecognized coordinate value'}
     END
  ENDCASE
  
  
  ;;=========================================================
  ;; Go about filling up new structure
  vnew = replicate( {l:0.d,$
                     b:0.d,$
                     vlsr:0.d,$
                     lw:0.d,$
                     tmb:0.d,$
                     sigt:0.}, count)
  
  tnames = tag_names(vs)
    
  ;; Copy over to the new structure
  vnew.l = l
  vnew.b = b
  
  ;; VLSR
  ti = where(tnames EQ v.vlsr, nt)
  IF nt EQ 0 THEN BEGIN
     message,'Error: VLSR name '+v.vlsr+' not valid',/cont
     count = 0
     RETURN,{error:'ERROR: FLAG NAME '+v.vlsr+'not valid'}
  ENDIF
  vnew.vlsr = vs.(ti) 
  
  ;; LW
  ti = where(tnames EQ v.lw, nt)
  IF nt EQ 0 THEN BEGIN
     message,'Error: LW name '+v.lw+' not valid',/cont
     count = 0
     RETURN,{error:'ERROR: FLAG NAME '+v.lw+'not valid'}
  ENDIF
  vnew.lw = vs.(ti) 
  
  ;; TMB
  ti = where(tnames EQ v.tmb, nt)
  IF nt EQ 0 THEN BEGIN
     message,'Error: TMB name '+v.tmb+' not valid',/cont
     count = 0
     RETURN,{error:'ERROR: FLAG NAME '+v.tmb+'not valid'}
  ENDIF
  vnew.tmb = vs.(ti) 
  
  ;; e_TMB
  ti = where(tnames EQ v.sigt, nt)
  IF nt EQ 0 THEN BEGIN
     message,'Error: SIGT name '+v.sigt+' not valid',/cont
     count = 0
     RETURN,{error:'ERROR: FLAG NAME '+v.sigt+'not valid'}
  ENDIF
  vnew.sigt = vs.(ti) 
  
  
  ;; Set detection flags, and cull out non-detections
  IF v.flag EQ '' THEN flag = replicate(1b,count) ELSE BEGIN
     ti = where(tnames EQ v.flag, nt)
     IF nt EQ 0 THEN BEGIN
        message,'Error: FLAG name '+v.flag+' not valid',/cont
        count = 0
        RETURN,{error:'ERROR: FLAG NAME '+v.flag+'not valid'}
     ENDIF
     flag = byte(vs.(ti))
  ENDELSE
  ind = WHERE(flag EQ 1 OR flag EQ 3, count)
  IF count NE 0 THEN vnew = vnew[ind] ELSE message,'RUM!  Stat!'
  
  ;; Return
  RETURN,vnew
END
