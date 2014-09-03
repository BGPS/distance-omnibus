;+
; NAME:
;       OMNI_GENERATE_EMAF
;
; PURPOSE:
;       Generate a list of EMAF sources in the catalog, and create an
;       IDL structure containing relevant information for those
;       sources.
;
; CATEGORY:
;       distance-omnibus EMAF routine
;
; CALLING SEQUENCE:
;       OMNI_GENERATE_EMAF [,CONFFILE=cfile]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE  [IDL Save file: ./local/(SURVEY)_emaf.sav]
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  10/23/12, TPEB -- Initial version, coppied and
;                                   significantly culled from
;                                   create_bgps_rb3_struct.pro.
;       Modified: 10/30/12, TPEB -- Added grs_angsep elements to the
;                                   EMAF structure.
;       Modified: 11/01/12, TPEB -- Check for the existance of
;                                   morph_distances.sav and remove --
;                                   that way, any update of the source
;                                   list will cause irdc_morph to be
;                                   re-run.
;       Modified: 11/02/12, TPEB -- Restrict sources to C >= 0.05, and
;                                   I_MIR <= 100 MJy/sr.
;       Modified: 11/29/12, TPEB -- Added SFA structure element to
;                                   encode star-formation activity
;                                   level from Dunham et al (2011,
;                                   ApJ, 731, 90).
;       Modified: 03/05/13, TPEB -- In the shift to OMNI_*.pro
;                                   code and generalized configuration
;                                   file input: name change and made
;                                   compatible with the new
;                                   framework.  Removed many
;                                   BGPS-specific features.
;       Modified: 03/20/13, TPEB -- Added CONFFILE optional input for
;                                   conformity with other routines.
;       Modified: 08/05/13, TPEB -- Added check for FINITE contrast.
;       Modified: 08/22/13, TPEB -- Added GRS 13CO kinematic
;                                   information, based on the
;                                   criteria determined from the
;                                   Edmonton2013 working group.  13CO
;                                   velocity will ONLY be used if a
;                                   dense gas velocity is absent.
;       Modified: 10/23/13, TPEB -- Relaxed the requirement for
;                                   velocity is the SURVEY does not
;                                   have kinematic information.  If
;                                   the SURVEY does have kinematic
;                                   information, this routine requires
;                                   a vlsr.
;       Modified: 10/24/13, TPEB -- Allowed for the non-use of the GRS
;                                   13CO data.
;       Modified: 12/19/13, TPEB -- Name change APERTURE_WTMASK ->
;                                   OMNI_APERTURE_MASK to align with the
;                                   rest of the code base, and
;                                   because I'm neurotic. Also,
;                                   set aperture diameter based on
;                                   SURVEY solid angle.
;       Modified: 01/09/14, TPEB -- Removed use of VEL_BLOCK
;
;-

PRO OMNI_GENERATE_EMAF, TEST=test, CONFFILE=cfile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Parse keywords
  test = KEYWORD_SET(test)
  
  ;; Read in config files
  conf = omni_load_conf(cfile)
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN
  ENDIF
  
  
  ;;====================================================================
  ;; Read in information relevant to this routine
  
  ;; Read in velocity structure, if necessary
  IF conf.hasvelocity THEN BEGIN
     vfn = './local/'+conf.survey+'_velocities.sav'
     IF ~FILE_TEST(vfn,/READ) THEN BEGIN
        message,'No velocity structure save file present.  '+$
                'Running OMNI_IMPORT_VELOCITY...',/inf
        omni_import_velocity
     ENDIF
     restore,vfn,/ver
     v_std = findgen(conf.nvbin) * conf.deltav + conf.vstart
  ENDIF
  
  rejlist = './local/'+conf.survey+'_reject_emaf.txt'
  IF ~FILE_TEST(rejlist,/READ) THEN BEGIN
     message,'Error: The EMAF rejection list '+rejlist+' does not exist.  '+$
             'Run OMNI_MIR_EXAMINE to create a by-eye source rejection list.',$
             /cont
     ;; Create empty file and go on... hopefully this works!
     openw,lun,rejlist,/get_lun
     close,lun
     free_lun,lun
  ENDIF
  
  ;; Read in SURVEY structure
  s   = omni_read_cat(conf.cat,ncat,fmt)
  fmt2 = string(ceil(alog10(ncat+1)),format="('I',I0)")
  sfwhm = string(conf.fwhm,format="(I0)") ; String for filenames
  
  
  IF conf.hasvelocity THEN BEGIN
     ;;==================================================================
     ;; Determine sources w/ (dense-gas vlsr OR GRS vlsr) && 
     ;;   overlap with GLIMPSE
     dgind = WHERE( v.vlsr NE -1000., ndg ) ; Dense gas VLSR
     
     ;; If using GRS...
     IF conf.usegrs THEN BEGIN
        coind = WHERE( v.grs.tpk NE 0.,  nco ) ; GRS 13CO  VLSR
        comi  = MISSING(dgind,coind,nmi) ; Use the Dense Gas, if we have it!!
        IF nmi NE 0 THEN coind = coind[comi] ; 13CO w/ NO dense gas
        
        velind = [dgind,coind]                       ; Combine the two lists
        veltyp = [replicate(1,ndg),replicate(2,nmi)] ; 1 = DG, 2 = 13CO
     ENDIF ELSE BEGIN
        velind = dgind
        veltyp = replicate(1,ndg)
     ENDELSE
     sortiv = sort(velind)      ; Sort into monatonic order
     velind = velind[sortiv]    ; VELIND sorted
     veltyp = veltyp[sortiv]    ; VELTYP sorted
     
     v = v[velind]              ; Select the objects with either DG OR 13CO
  ENDIF ELSE v = s              ; For SURVEY without velocity structure
  
  ;; Check which objects have EMAF postage-stamp images from GLIMPSE
  EMAF_DIR = local.emafpost
  n_gpc = 0
  WHILE n_gpc EQ 0 DO BEGIN
     ufn = FILE_SEARCH(EMAF_DIR+conf.survey+'_mapdat*.fits', COUNT=n_gpc)
     ;; If no files exist, then run OMNI_GLIMPSE_EMAF
     IF n_gpc EQ 0 THEN BEGIN
        message,'Warning:  No EMAF-processed GLIMPSE postage stamps present. '+$
                'Running OMNI_GLIMPSE_EMAF...',/inf
        omni_glimpse_emaf
     ENDIF
  ENDWHILE
  proc_cnum = long(strmid(ufn,STRPOS(ufn[0],'mapdat')+6,4)) ; Processed Objs
  
  ;; Get the overlap between velocity tracers and the EMAF GLIMPSE proc
  ind = WHERE_ARRAY(v.cnum, proc_cnum, n)
  ufn = ufn[ind]
  
  ;; Work backwards to get EMAF GLIMPSE proc with VLSR...
  ind2 = WHERE_ARRAY(proc_cnum, v.cnum, n2)
  v = v[ind2]
  IF n NE n2 THEN $
     message,"Take a shot of gin, then figure out the size mismatch."
  
  print,'Total sample is '+string(n,format="(I0)")+' objects.'
  
  
  
  ;;=======================================================================
  ;; Create a structure to hold the information relevant to selected EMAFs
  
  emaf = replicate( {name:'',$             ; Object name
                     cnum:0,$              ; Catalog number
                     l:0.,$                ; GLON
                     b:0.,$                ; GLAT
                     vlsr:0.,$             ; VLSR
                     v_flag:0b,$           ; Flag specifying which VLSR 
                     s_ap:0.,$             ; Flux density in beam-equivalent ap.
                     s_ap_err:0.,$         ; Error in above
                     I_mir:0.,$            ; I_MIR (total along LOS)
                     I_min:0.,$            ; Minimum intensity in EMAF
                     C:-1000.,$            ; Contrast
                     sig_imir:0.,$         ; Error in I_MIR
                     sig_imin:0.,$         ; Error I_min
                     delta_imir:0.,$       ; Spread in I_MIR values across p.s.
                     sig_c:0.,$            ; Error in C
                     n200:0}, n)           ; Number of pixels above 200 MJy/sr
  
  ;; Find tag # in s corresponding to 1st flux density aperture and
  ;;   associated error -- THIS IS REQUIRED!!!
  IF ~TAG_EXIST(s,conf.s[1].val,INDEX=iflux) || $
     ~TAG_EXIST(s,conf.s[1].err,INDEX=ieflux) THEN BEGIN
     message,'Error: Tags specified in survey-info config file for 1st flux '+$
             'density aperture are not present in the catalog.  Exiting.',/cont
     RETURN
  ENDIF
  
  
  
  ;;====================================================================
  ;; Now, loop through each potential source...
  FOR i=0L, n-1 DO BEGIN
     
     ;; Appraise the user of status
     IF ((i+1) MOD 100) EQ 0 THEN message,'Computing properties of object #'+$
                                          string(i+1,format='('+fmt2+')')+$
                                          ' of '+$
                                          string(n,format='('+fmt2+')'),/inf
     
     ;; Since v and proc_cnum are now the same size, loop through
     ;; v, since its cnum are in proper order
     j = where(proc_cnum EQ v[i].cnum, nj) ; Element of proc_cnum
     k = where(s.cnum EQ v[i].cnum, nk)    ; Element of the SURVEY catalog
     IF nj EQ 0 THEN message,$
        "Take a shot of rum, then figure out why this object isn't in proc."
     
     ;; Start filling in the emaf structure from the SURVEY catalog
     emaf[i].name = s[k].name
     emaf[i].cnum = s[k].cnum
     emaf[i].l    = s[k].glon
     emaf[i].b    = s[k].glat
     
     ;; INSERT PROPER FLUX MEASUREMENT HERE -- structure expects Jy
     emaf[i].s_ap      = s[k].(iflux) * double(conf.fluxcor)
     sig_ss            = (s[k].(ieflux) > conf.noisefloor) * $
                         double(conf.fluxcor)
     emaf[i].s_ap_err  = sqrt(sig_ss*sig_ss + emaf[i].s_ap * emaf[i].s_ap * $
                              conf.fluxcorerr*conf.fluxcorerr)
     
     IF conf.hasvelocity THEN BEGIN
        ;; Insert VLSR from v
        CASE veltyp[i] OF
           1: BEGIN             ; Dense Gas
              emaf[i].vlsr   = v[i].vlsr
              emaf[i].v_flag = v[i].rv_types
           END
           2: BEGIN             ; GRS 13CO
              emaf[i].vlsr   = v[i].grs.vlsr
              emaf[i].v_flag = -1 ; Flag = -1 FOR GRS 13CO
           END
        ENDCASE
     ENDIF
     
     ;;=============================================================
     ;; Cull based on latitude + location within the L-V diagram:
     ;; Check latitude -- needs to be within safe boundaries of GLIMPSE
     IF (abs(emaf[i].b) GT 1. || emaf[i].l LT -65.0 || $
         (emaf[i].l GT 65.25 && emaf[i].l LT 295.0)) THEN CONTINUE
     
     IF conf.hasvelocity THEN BEGIN
        ;; Check on the longitude range -- can't use objects w/in 21
        ;;    deg of GC UNLESS part of Molecular Ring structure
        IF (emaf[i].l LT 21.) THEN $
           IF (emaf[i].vlsr GT (10.d/3.d*emaf[i].l + 15.d)) OR $
           (emaf[i].vlsr LT (20.d/9.d*emaf[i].l - 50.d/3.d)) THEN CONTINUE
        
        ;; NOTE: THIS CHECK DOES NOT ACCOUNT FOR THE STRUCTURE OF THE
        ;; MOLECULAR RING IN THE 4TH QUADRANT!!!  AS CODED, THIS
        ;; CONSTRAINT IS DESIGNED FOR THE BGPS (l >= 7.5)!!!
     ENDIF
     
     
     ;;=============================================================
     ;; Next, load UBC postage stamp images (created by
     ;; process_glimpse_irdc.pro) and insert mid-infrared data into
     ;; the emaf structure.
     
     scnum = string(s[k].cnum,format=fmt)
     imir_img = readfits(EMAF_DIR+conf.survey+'_Imir'+scnum+'.fits',$
                         imir_hdr,/silent)
     nons_img = readfits(EMAF_DIR+conf.survey+'_nonsmo'+scnum+'.fits',$
                         nons_hdr,/silent)
     smoo_img = readfits(EMAF_DIR+conf.survey+'_s'+sfwhm+'arc'+scnum+'.fits',$
                         smoo_hdr,/silent)
     
     ;; For postage-stamp images going off the edge of the GLIMPSE
     ;; map, replace 0's with NANs -- which can more easily be
     ;; dealt with later.
     inan = WHERE(nons_img EQ 0, nnan)
     IF nnan THEN BEGIN
        imir_img[inan] = !values.f_nan
        nons_img[inan] = !values.f_nan
        smoo_img[inan] = !values.f_nan
     ENDIF
     
     ;; TO CALCULATE I_MIR & I_min (with their uncertainties), use the
     ;; aperture with equivalent solid angle of the SURVEY beam.
     ap_size = double(ceil(sqrt(conf.omega/!dpi)*2.*206265.))
     wtmask  = omni_aperture_mask(smoo_hdr, ap_size, emaf[i].l, emaf[i].b)
     ind     = where(wtmask GT 0.5)
     
     ;; WTMASK2: 4' in diameter
     wtmask2 = omni_aperture_mask(imir_hdr, 240., emaf[i].l, emaf[i].b)
     ind2    = where(wtmask2 GT 0)
     
     ;; Calculate fraction of nonsmooth with I >= 200 MJy/sr
     ;;   w/in 2' of peak location
     if200 = where((nons_img*wtmask2) GE 200., n200)
     emaf[i].n200 = n200
     
     ;; Calculate the extracted values
     emaf[i].I_min = min( smoo_img[ind], /NAN )
     emaf[i].I_MIR = total( wtmask2*imir_img,/NAN ) / total(wtmask2)
     
     IF ~finite(emaf[i].I_MIR) THEN STOP
     
     ;; Calculate the uncertainties
     emaf[i].sig_imin = stddev( smoo_img[ind], /NAN )
     
     V1 = total(wtmask2,/NAN)
     V2 = total(wtmask2*wtmask2,/NAN)
     emaf[i].sig_imir = sqrt(total(wtmask2[ind2]*$
                                   (imir_img[ind2]-emaf[i].I_mir)^2,/NAN) * $
                             (V1 / (V1*V1 - V2)) )
     emaf[i].delta_imir = max(imir_img,/NAN) - min(imir_img,/NAN)
     
     ;; Compute the contrast
     emaf[i].c = ( 1.d - (emaf[i].I_min / emaf[i].I_mir) )
     emaf[i].sig_C  = sqrt( (emaf[i].i_min*emaf[i].i_min/$
                             emaf[i].i_mir/emaf[i].i_mir)*$
                            (emaf[i].sig_imir*emaf[i].sig_imir/$
                             emaf[i].i_mir/emaf[i].i_mir + $
                             emaf[i].sig_imin*emaf[i].sig_imin/$
                             emaf[i].i_min/emaf[i].i_min) )
     
  ENDFOR                        ; End of source loop
  
  
  
  ;; Save full list for comparisons, as needed
  IF test THEN save,emaf,filename='./local/'+conf.survey+'_emaf_uncut.sav',/ver
  
  
  ;; Check for POSITIVE mid-infrared contrast (C >= 0.05)
  ;; Check for sources with suspicious VLSR ( < -30 km/s)
  ;;=========>>  IN THE 1ST QUADRANT!!!  This needs to be augmented
  ;;=========>>  for surveys using the 4th Quadrant. 
  ;; Check for I_MIR <= 100 MJy/sr
  ;; Check for I_8um >= 200 MJy/sr w/in 2' (n200 < 10 pixels)
  goodind = WHERE( (emaf.c GT 0.05) AND (emaf.vlsr GE -30.) AND $
                   (emaf.I_MIR LE 100.) AND (emaf.n200 LT 10), ngood)


  ;; goodind = WHERE( (emaf.c GT 0.05) AND (emaf.vlsr GE -30.) AND $
  ;;                  (emaf.I_MIR LE 100.) AND (emaf.n200 LT 10) AND $
  ;;                  finite(emaf.c), ngood)
  emaf = emaf[goodind]
  
  IF test THEN $
     save,emaf,filename='./local/'+conf.survey+'_emaf_potential.sav',/ver
  print,'Number of potentially useable objects: ',n_elements(emaf)
  
  ;; Check against BY-EYE REJECTION list...
  readcol,rejlist,reject_cnum,format='I',comment='#'
  IF n_elements(reject_cnum) THEN BEGIN
     good_emaf_ind = MISSING(reject_cnum, emaf.cnum, n_good_emaf)
     print,'# Good: ',n_good_emaf
     emaf = emaf[good_emaf_ind]
  ENDIF ELSE $
     print,conf.survey+'_reject_emaf.txt file empty.  Using all '+$
           string(n_elements(emaf),format="(I0)")+' objects'
  
  ;;====================================================================
  ;; We now have the final list of sources!
  ;; Save the structure
  save,emaf,filename='./local/'+conf.survey+'_emaf.sav',/ver
  
  ;; Clear out the memory a liitle
  undefine,v,s,emaf
  
  RETURN
END
