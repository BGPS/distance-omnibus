;+
; NAME:
;       OMNI_READ_CONFFILE_SURVEY
;
; PURPOSE:
;       Parses the SURVEY-type configuration file.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE_SURVEY( lun, conffile, lnum )
;
; INPUTS:
;       LUN      -- The logical unit number for the configuration file.
;       CONFFILE -- Name of the configuration file, used only for
;                   error display.
;       LNUM     -- Line number, used only for error display.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       CONF -- Structure containing the fields in the configuration
;               file.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  02/19/13, TPEB -- Initial version.
;       Modified: 02/21/13, TPEB -- Split out parsing of individual
;                                   config types into subfunctions.
;       Modified: 02/25/13, TPEB -- Adding CASE statements for the
;                                   velocity information
;       Modified: 02/26/13, TPEB -- Added conf.pixscale and
;                                   conf.ppbeam for SURVEY.  Added
;                                   LOCAL type parsing.
;       Modified: 02/27/13, TPEB -- Moved key/val extraction to new
;                                   subroutine, added GALACTIC type
;                                   parsing.
;       Modified: 03/04/13, TPEB -- Added FWHM keyword to SURVEY type
;                                   config file.
;       Modified: 06/05/13, TPEB -- Added noise/hasnoise to SURVEY
;                                   type.
;       Modified: 08/26/13, TPEB -- Added label map flag to
;                                   SURVEY. Also, force check of nword
;                                   EQ 1.
;       Modified: 10/23/13, TPEB -- Added hasvelocity and mirmult and
;                                   usegrs fields to SURVEY.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;
;-

;;=========================================================================
;; Function for the parsing of survey-info configuration files.
;; v[?].coord is an integer corresponding to type:
;;      21 = Galactic Coords
;;      22 = J2000 Celestial Coords
;;      23 = B1950 Celestial Coords
FUNCTION OMNI_READ_CONFFILE_SURVEY, lun, conffile, lnum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Create the velocity information structure
  vstr = replicate( {has:0b,$
                     coord:0b,$
                     name:'',$
                     mrt:'',$
                     flag:'',$
                     vlsr:'',$
                     lw:'',$
                     tmb:'',$
                     sigt:''}, 20)
  
  ;; Create the configuration structure for TYPE survey
  conf = {survey:'',$
          cat:'',$
          hasmap:0b,$
          haslabel:0b,$
          hassmooth:0b,$
          hascrop:0b,$
          hasnoise:0b,$
          fwhm:0.,$
          omega:0.d,$
          nu:0.d,$
          kappa:0.d,$
          ppbeam:0.,$
          fluxcor:1.,$    ; Surveys other then BGPS_V1 don't need this keyword
          fluxcorerr:0.,$ ; Ditto above
          noisefloor:0.,$
          maps:'',$
          label:'',$
          smooth:'',$
          crop:'',$
          noise:'',$
          complete:0.d,$
          labvaliscnum:0b,$
          mirmult:1.d,$
          hasvelocity:0b,$
          usegrs:0b,$
          v:vstr,$
          s:replicate({has:0b,val:'',err:'',size:0.},10),$
          maxdist:0.,$
          nvbin:0,$
          deltav:0.,$
          vstart:0.,$
          error:''}
  
  ;; Continue reading the configuration file
  WHILE ~EOF(lun) DO BEGIN
     
     key = OMNI_READ_CONFFILE_READLINE(lun, conffile, lnum, val, nword, skip)
     IF skip THEN CONTINUE
     
     ;; Parse Velocity keywords
     IF strlowcase(strmid(key,0,1)) EQ 'v' THEN BEGIN
        vi = fix(strmid(key,1,1))-1
        
        ;; If no value set, break
        IF nword NE 2 THEN BREAK
        
        CASE strmid(key,2) OF
           'name':BEGIN
              conf.v[vi].has = 1b
              conf.v[vi].name = val
           END
           'mrt':  conf.v[vi].mrt  = val
           'flag': conf.v[vi].flag = strupcase(val)
           'vlsr': conf.v[vi].vlsr = strupcase(val)
           'lw':   conf.v[vi].lw   = strupcase(val)
           'tmb':  conf.v[vi].tmb  = strupcase(val)
           'sigt': conf.v[vi].sigt = strupcase(val)
           'coord': CASE strlowcase(val) OF
              ;; Set integer values corresponding to coordinate type
              'galactic': conf.v[vi].coord = 21
              'j2000':    conf.v[vi].coord = 22
              'b1950':    conf.v[vi].coord = 23
              '':         conf.v[vi].has = 0b ; If no coord, then no vel
              ELSE: BEGIN
                 message,'Coordinate system '+val+' in '+key+' not recognized',$
                         /cont
                 RETURN,{survey:'ERROR:',cat:'',$
                         error:'Bad coordinate system in '+key}
              END
           ENDCASE
           ELSE:BEGIN
              message,'Unknown keyword >'+key+'< on line '+$
                      strtrim(lnum,2)+' in conffile '+conffile,/cont
              RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
           END
        ENDCASE
     ENDIF ELSE $               ; Parse flux density keywords
        IF strlowcase(strmid(key,0,4)) EQ 'flux' THEN BEGIN
        fi = fix(strmid(key,4,1))
        IF nword NE 2 || val EQ '' THEN CONTINUE
        conf.s[fi].has = 1b
        conf.s[fi].val = val
     ENDIF ELSE $               ; Parse flux density error keywords
        IF strlowcase(strmid(key,0,5)) EQ 'eflux' THEN BEGIN
        fi = fix(strmid(key,5,1))
        IF nword NE 2 || val EQ '' THEN CONTINUE
        conf.s[fi].err = val
     ENDIF ELSE $               ; Parse aperture size keywords
        IF strlowcase(strmid(key,0,4)) EQ 'aper' THEN BEGIN
        fi = fix(strmid(key,4,1))
        IF nword NE 2 || val EQ '' THEN CONTINUE
        conf.s[fi].size = val
     ENDIF ELSE BEGIN
        
        ;; Else parse other keywords into structure
        CASE key OF
           'survey':   conf.survey = val
           'cat':      conf.cat    = val
           
           ;;===============================
           ;; Map data product keywords
           'maps': BEGIN
              IF nword NE 2 THEN BREAK
              IF ~FILE_TEST(val,/read) THEN BREAK
              conf.hasmap = 1b
              conf.maps = val
           END
           'label': BEGIN
              IF nword NE 2 THEN BREAK
              IF ~FILE_TEST(val,/read) THEN BREAK
              conf.haslabel = 1b
              conf.label = val
           END
           'smooth': BEGIN
              IF nword NE 2 THEN BREAK
              IF ~FILE_TEST(val,/read) THEN BREAK
              conf.hassmooth = 1b
              conf.smooth = val
           END
           'noise': BEGIN
              IF nword NE 2 THEN BREAK
              IF ~FILE_TEST(val,/read) THEN BREAK
              conf.hasnoise = 1b
              conf.noise = val
           END
           'crop': BEGIN
              IF nword NE 2 THEN BREAK
              IF ~FILE_TEST(val,/read) THEN BREAK
              conf.hascrop = 1b
              conf.crop = val
           END
           
           ;;===============================
           ;; Other keywords
           'fwhm':        conf.fwhm          = (nword EQ 1) ? 0.  : float(val)
           'omega':       conf.omega         = (nword EQ 1) ? 0.d : double(val)
           'nu':          conf.nu            = (nword EQ 1) ? 0.d : double(val)
           'kappa':       conf.kappa         = (nword EQ 1) ? 0.d : double(val)
           'ppbeam':      conf.ppbeam        = (nword EQ 1) ? 0.  : float(val)
           'corflux':     conf.fluxcor       = (nword EQ 1) ? 1.  : float(val)
           'errcorflux':  conf.fluxcorerr    = (nword EQ 1) ? 0.  : float(val)
           'noisefloor':  conf.noisefloor    = (nword EQ 1) ? 0.  : float(val)
           'complete':    conf.complete      = (nword EQ 1) ? 0.d : double(val)
           'labvaliscnum':conf.labvaliscnum  = (nword EQ 1) ? 0b  : long(val)
           'mirmult':     conf.mirmult       = (nword EQ 1) ? 1.  : float(val)
           'hasvelocity': conf.hasvelocity   = (nword EQ 1) ? 0b  : long(val)<1
           'usegrs':      conf.usegrs        = (nword EQ 1) ? 0b  : long(val)<1
           'maxdist':     conf.maxdist       = (nword EQ 1) ? 0.  : float(val)
           'nvbin':       conf.nvbin         = (nword EQ 1) ? 0L  : long(val)
           'deltav':      conf.deltav        = (nword EQ 1) ? 0.  : float(val)
           'startv':      conf.vstart        = (nword EQ 1) ? 0.  : float(val)
           ELSE:BEGIN
              message,'Unknown keyword >'+key+'< on line '+$
                      strtrim(lnum,2)+' in conffile '+conffile,/cont
              RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
           END
        ENDCASE
     ENDELSE
  ENDWHILE
  
  RETURN,conf
END
