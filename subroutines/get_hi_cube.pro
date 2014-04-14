;+
; NAME:
;       GET_HI_CUBE
;
; PURPOSE:
;       Returns the filename of the HI data cube corresponding to a
;       particular (l,b).
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       filename = GET_HI_CUBE( glon, glat [,hi_dirs][,cont_fn])
;
; INPUTS:
;       GLON     -- Galactic longitude of pointing(s) (scalar or array)
;       GLAT     -- Galactic latitude of pointing(s)  (scalar or array)
;
; OPTIONAL INPUTS:
;       HI_DIRS  -- 3-element array containing the ames of the
;                   directories containing HI data (Default values
;                   found in local_layout.conf)
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       FILENAME -- Filename of the desired HI data cube.  If
;                   coordinates are outside IGPS coverage, then an
;                   empty string is returned.
;
; OPTIONAL OUTPUTS:
;       CONT_FN  -- Filename of the associated 21-cm continuum image
;                   (VGPS and CGPS only).  (Same caveat as above.)
;       SURVEY   -- Byte array (one for each element of glon / glat)
;                   referencing the IGPS survey for that coordinate.
;                   Values: [0: NOT DEFINED,, 1: SGPS, 2: VGPS, 3: CGPS]
;
; COMMON BLOCKS:
;       OMNI_CONFIG    -- The set of configuration structures, read in
;                         from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  10/21/10, TPEB -- Split routine into separate file
;                                   from hisa.pro for use with making
;                                   postage stamp images integrated
;                                   over HI data cubes.
;       Modified: 04/28/11, TPEB -- Added OPTIONAL OUTPUT survey for
;                                   returning the IGPS survey member
;                                   of each filename.
;       Modified: 04/29/11, TPEB -- Fixed bug in the SGPS section
;                                   where it was looking for the 'old'
;                                   filenames.
;       Modified: 06/23/11, TPEB -- Added entire SGPS data set to the
;                                   lookup table.  Also, routine now
;                                   returns blank string if
;                                   coordinates are outside IGPS
;                                   coverage.
;       Modified: 09/19/13, TPEB -- Modernized the code for OMNI_* and
;                                   replaced IF ... ELSE IF cascades
;                                   with CASE statements.
;       Modified: 10/14/13, TPEB -- Bug fixes upon actual use of above
;                                   revisions.
;
;-

FUNCTION GET_HI_CUBE, glon, glat, hi_dirs, cont_fn, survey, CONFFILE=cfile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Parse input
  conf = omni_load_conf(cfile)
  
  ;; If using default hi_dirs, then extract from configuration file
  IF n_elements(hi_dirs) EQ 0 THEN BEGIN
     local = omni_read_conffile('./conffiles/local_layout.conf')
     
     readcol, local.sgps, sgps_fn, comment='#', format='a'
     sgps = strmid(sgps_fn[0],0,strpos(sgps_fn[0],'/',/reverse_search)+1)
     
     readcol, local.vgps, vgps_fn, comment='#', format='a'
     vgps = strmid(vgps_fn[0],0,strpos(vgps_fn[0],'/',/reverse_search)+1)
     
     readcol, local.cgps, cgps_fn, comment='#', format='a'
     cgps = strmid(cgps_fn[0],0,strpos(cgps_fn[0],'/',/reverse_search)+1)
     
  ENDIF ELSE BEGIN              ; Else, extract from input
     
     hi_match = STRLOWCASE(hi_dirs)
     sgps = hi_dirs[where(strmatch(hi_match,'*sgps*') eq 1,nmatch)]
     vgps = hi_dirs[where(strmatch(hi_match,'*vgps*') eq 1,nmatch)]
     cgps = hi_dirs[where(strmatch(hi_match,'*cgps*') eq 1,nmatch)]
     
  ENDELSE
  
  ;; Set up other arrays, etc.
  n_pts   = n_elements(glon)
  fnames  = strarr(n_pts)
  cont_fn = strarr(n_pts)
  survey  = bytarr(n_pts)
  
  FOR i=0L, n_pts-1 DO BEGIN
     
     ;; Check for GPS from which to pull the file
     CASE 1 OF
        
        ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;; SGPS
        (glon[i] LT 18 OR glon[i] GE 253): BEGIN  
           survey[i] = 1b
           
           IF glon[i] LE 180 THEN BEGIN                   ;; QUADRANT I
              IF glon[i] GE 14 THEN BEGIN
                 fnames[i] = sgps + 'g015.hi.fits'
                 cont_fn[i] = sgps + 'cont_g015.hi.fits'
              ENDIF ELSE BEGIN
                 fnames[i] = sgps + 'g010.hi.fits'
                 cont_fn[i] = sgps + 'cont_g010.hi.fits'
              ENDELSE
           ENDIF ELSE BEGIN                               ;; QUADRANTS IV & III
              IF glon[i] LE 350 THEN BEGIN
                 fnames[i]  = sgps + string(round((glon[i]-258)/10.)*10.+258,$
                                            format="('g',I03,'.hi.fits')")
                 cont_fn[i] = sgps + string(round((glon[i]-258)/10.)*10.+258,$
                                            format="('cont_g',I03,'.hi.fits')")
              ENDIF ELSE BEGIN
                 fnames[i]  = sgps + 'g353.hi.fits'
                 cont_fn[i] = sgps + 'cont_g353.hi.fits'
              ENDELSE
           ENDELSE
        END           
        
        ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;; VGPS
        (glon[i] GE 18 AND glon[i] LE 67): BEGIN
           survey[i] = 2b
           
           ;; Cubes are 5 deg long, centered on (glon - 17) 4's
           fnames[i]  = vgps + string(round((glon[i]-17)/4.)*4.+17,$
                                      format="('MOS_',I03,'.Tb.fits')")
           cont_fn[i] = vgps + string(round((glon[i]-17)/4.)*4.+17,$
                                      format="('MOS_',I03,'_cont.Tb.fits')")
        END
        
        ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;; CGPS
        (glon[i] GT 67 AND glon[i] LE 175): BEGIN                    ;; CGPS
           survey[i] = 3b
           
           CASE 1 OF            ; Use CASE statement to assign CGPS_FIELD
              (glon[i] GT 150.8): cgps_field = 'MST1'
              (glon[i] GT 146.8 AND glon[i] LE 150.8): $
                 cgps_field = glat[i] LT 1. ? 'MU1' : 'MU2'
              (glon[i] GT 142.8 AND glon[i] LE 146.8): $
                 cgps_field = glat[i] LT 1. ? 'MV1' : 'MV2'
              (glon[i] GT 138.8 AND glon[i] LE 142.8): $
                 cgps_field = glat[i] LT 1. ? 'MW1' : 'MW2'
              (glon[i] GT 134.8 AND glon[i] LE 138.8): $
                 cgps_field = glat[i] LT 1. ? 'MX1' : 'MX2'
              (glon[i] GT 130.8 AND glon[i] LE 134.8): $
                 cgps_field = glat[i] LT 1. ? 'MY1' : 'MY2'
              (glon[i] GT 126.8 AND glon[i] LE 130.8): $
                 cgps_field = glat[i] LT 1. ? 'MA1' : 'MA2'
              (glon[i] GT 122.8 AND glon[i] LE 126.8): $
                 cgps_field = glat[i] LT 1. ? 'MB1' : 'MB2'
              (glon[i] GT 118.8 AND glon[i] LE 122.8): $
                 cgps_field = glat[i] LT 1. ? ' MC1' : 'MC2'
              (glon[i] GT 114.8 AND glon[i] LE 118.8): cgps_field = 'MD1'
              (glon[i] GT 110.8 AND glon[i] LE 114.8): cgps_field = 'ME1'
              (glon[i] GT 106.8 AND glon[i] LE 110.8): cgps_field = 'MF1'
              (glon[i] GT 102.8 AND glon[i] LE 106.8): cgps_field = 'MG2'
              (glon[i] GT 98.8 AND glon[i] LE 102.8) : cgps_field = 'MH2'
              (glon[i] GT 94.8 AND glon[i] LE 98.8)  : cgps_field = 'MIJ2'
              (glon[i] GT 90.8 AND glon[i] LE 94.8)  : cgps_field = 'MK1'
              (glon[i] GT 86.8 AND glon[i] LE 90.8)  : cgps_field = 'ML1'
              (glon[i] GT 82.8 AND glon[i] LE 86.8)  : cgps_field = 'MM1'
              (glon[i] GT 78.8 AND glon[i] LE 82.8)  : cgps_field = 'MN1'
              (glon[i] GT 74.8 AND glon[i] LE 78.8)  : cgps_field = 'MO1'
              (glon[i] GT 70.8 AND glon[i] LE 74.8)  : cgps_field = 'MP1'
              (glon[i] GT 67 AND glon[i] LE 70.8)    : cgps_field = 'MQ1'
              
              
              
              ;; HTML CODE BELOW LEFT IN FOR FUTURE EXPANSIONS OF THE BGPS
              
              ;; <OPTION VALUE="MEV1">170.2&lt;l&lt;175.3,-3.5&lt;b&lt;+1.5&nbsp;&nbsp;&nbsp; MEV1
              ;; <OPTION VALUE="MEV2">170.2&lt;l&lt;175.3,+0.5&lt;b&lt;+5.5&nbsp;&nbsp;&nbsp; MEV2
              ;; <OPTION VALUE="MEW1">166.2&lt;l&lt;171.3,-3.5&lt;b&lt;+1.5&nbsp;&nbsp;&nbsp; MEW1
              ;; <OPTION VALUE="MEW2">166.2&lt;l&lt;171.3,+0.5&lt;b&lt;+5.5&nbsp;&nbsp;&nbsp; MEW2
              ;; <OPTION VALUE="MEX1">162.2&lt;l&lt;167.3,-3.5&lt;b&lt;+1.5&nbsp;&nbsp;&nbsp; MEX1
              ;; <OPTION VALUE="MEX2">162.2&lt;l&lt;167.3,+0.5&lt;b&lt;+5.5&nbsp;&nbsp;&nbsp; MEX2
              ;; <OPTION VALUE="MEY1">158.2&lt;l&lt;163.3,-3.5&lt;b&lt;+1.5&nbsp;&nbsp;&nbsp; MEY1
              ;; <OPTION VALUE="MEY2">158.2&lt;l&lt;163.3,+0.5&lt;b&lt;+5.5&nbsp;&nbsp;&nbsp; MEY2
              ;; <OPTION VALUE="MEZ1">154.2&lt;l&lt;159.3,-3.5&lt;b&lt;+1.5&nbsp;&nbsp;&nbsp; MEZ1
              ;; <OPTION VALUE="MEZ2">154.2&lt;l&lt;159.3,+0.5&lt;b&lt;+5.5&nbsp;&nbsp;&nbsp; MEZ2
              ;; <OPTION VALUE="MST1">150.2&lt;l&lt;155.3,-3.5&lt;b&lt;+1.5&nbsp;&nbsp;&nbsp; MST1
              ;; <OPTION VALUE="MST2">150.2&lt;l&lt;155.3,+0.5&lt;b&lt;+5.5&nbsp;&nbsp;&nbsp; MST2
              
              ELSE: cgps_field = 'NOFIELD' ; Something to pitch an error
           ENDCASE
           
           fnames[i]  = cgps + 'CGPS_'+cgps_field+'_HI_line_image.fits'
           cont_fn[i] = cgps + 'CGPS_'+cgps_field+'_1420_MHz_I_image.fits'
        END
        
        ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;; Other / Unknown
        ELSE: BEGIN
           fnames[i] = ''
           cont_fn[i] = ''
        END
        
     ENDCASE
     
  ENDFOR
  
  RETURN,fnames
END
