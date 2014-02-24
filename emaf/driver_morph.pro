;+
; NAME:
;       DRIVER_MORPH
;
; PURPOSE:
;       Drive the IRDC morphological matching routine from somewhere
;       besides the DISTANCE_OMNIBUS routine.  This routine is
;       specifically designed for testing of the morphological
;       matching scheme before introduction into the general
;       distance-omnibus pipeline.
;
; CATEGORY:
;       Teasting and debugging
;
; CALLING SEQUENCE:
;       DRIVER_MORPH [,START=start][,REAR=rear][,CNUM_LIST=cnum_list][,/SAVE]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       *Note: (CNUM_LIST) and (START, REAR) are mutually exclusive
;              options: if CNUM_LIST is supplied, then START & REAR
;              are ignored. 
;       CNUM_LIST     -- List of BGPS catalog numbers for objects to
;                        calculate (as opposed to running the entire
;                        catalog).
;       START         -- First BGPS catalog number to process
;                        [Default: #1]
;       REAR          -- Last BGPS catalog number to process [Default:
;                        last entry in the catalog]
;
; KEYWORD PARAMETERS:
;       SAVE  -- Save the resulting PVEC into an IDL save file
;                ./irdc_dist_model/irdc_morph_pvec.sav (for debugging
;                purposes).
;
; OUTPUTS:
;       NONE (creates .EPS files morph????.eps in the ./local/output/
;       directory). 
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       VEL_BLOCK (needed for KDIST overlay)
;
; MODIFICATION HISTORY:
;
;       Created:  09/26/11, TPEB -- Initial version, for driving IRDC
;                                   morphological matching code from
;                                   Erik.
;       Modified: 12/21/11, TPEB -- Name change as part of repository
;                                   cleanup (was morph_driver.pro)
;       Modified: 02/27/12, TPEB -- Added VEL_BLOCK to the routine in
;                                   concert with changes to
;                                   IRDC_MORPH.pro as part of
;                                   debugging new probability scheme.
;       Modified: 07/02/12, TPEB -- Put extraneous keywords into the
;                                   _EXTRA keyword for passing through
;                                   to IRDC_MORPH.pro.
;       Modified: 10/26/12, TPEB -- Updated to work with the new EMAF
;                                   framework and Robitaille (2012)
;                                   numerical model for Galactic
;                                   ffore.
;       Modified: 11/02/12, TPEB -- Changed SAVE & RESTORE commands to
;                                   follow verbosity rule passed via
;                                   /SILENT.
;       Modified: 11/26/12, TPEB -- Changed default alpha to 3.
;       Modified: 11/27/12, TPEB -- Changed save file location to
;                                   ./emaf/save_files/, plus going
;                                   back to alpha = 2.
;
;-



PRO DRIVER_MORPH, START=start, REAR=rear, CNUM_LIST=cnum_list, SAVE=save, $
                  _EXTRA=_extra, FMEAS=fmeas, MDIST=mdist, ALPHA=alpha, $
                  SILENT=silent, NOPS=nops, FNSAVE=savefn
  
  ;; FORWARD_FUNCTION BGPS_FFORE_MODEL, BGPS_FFORE_LOOPIE, STAR_DENSITY
  ;; RESOLVE_ROUTINE, 'BGPS_FFORE', /COMPILE_FULL_FILE, /EITHER, /NO_RECOMPILE
  
  ;; defsysv, '!IRDC', exists = exists
  ;; IF ~ exists THEN irdc_params
  ;; print,!IRDC.A,!IRDC.Td
  
  ;; Parse Keywords
  silent  = KEYWORD_SET(silent)
  make_ps = ~ KEYWORD_SET(nops)
  save    = KEYWORD_SET( save )
  sendfmeas = (n_elements(fmeas) EQ 0) ? 0b : 1b
  sendmdist = (n_elements(mdist) EQ 0) ? 0b : 1b
  IF n_elements(alpha) EQ 0 THEN BEGIN
     alpha = 2.
     suff_alpha = ''
  ENDIF ELSE suff_alpha = string(alpha,format="('_',F0.2)")
  
  ;; Load in the BGPS catalog and Velocity Structure
  s = read_bgps_csv('bgps_distance_database.csv',csv,VERBOSE=~silent)
  COMMON VEL_BLOCK, velocity
  IF ~ exist(velocity) THEN $
     restore,'./local/bgps_velocity_struct.sav',VERBOSE=~silent
  
  ;; Get repository parameters
  defsysv, '!MW', exists = exists
  IF NOT exists THEN galactic_params 
  d = dindgen(!MW.NBINS)*!MW.BINSIZE + !MW.BINSTART
  
  n_bgps = csv.nrows
  start = 0L
  rear = csv.nrows-1
  
  ;; Set up the structure to hold the DPDFs
  pvec = replicate( create_struct('cnum',0L,'l',0.d,'b',0.d,'has',0b,$
                                  'invx',dblarr(n_elements(d))), n_bgps)
  pvec.cnum = s.cnum
  pvec.l    = s.glon_peak
  pvec.b    = s.glat_peak
  
  ;; If CNUM_LIST supplied, then only do the big loop for those objects
  IF n_elements( cnum_list ) NE 0 THEN BEGIN
     n_bgps = n_elements( cnum_list ) 
     ind = WHERE_ARRAY([cnum_list], s.cnum, nind)
     IF ~silent THEN print,'NIND: ',nind
     s = s[ind]
     start = 0L
     rear = n_bgps-1
  ENDIF ELSE BEGIN
     ;; Else, look for START & REAR keywords and adjust accordingly
     start = (n_elements(start) NE 0) ? long(start) : 0L
     rear = (n_elements(rear) NE 0) ? long(rear) : csv.nrows-1
  ENDELSE  
  
  ;; Loop through all BGPS sources to calculate IRDC association probabilities
  FOR i=start, rear DO BEGIN
     
     send  = sendfmeas ? fmeas[i] : 2.
     send2 = sendmdist ? mdist[i] : -1.
     
     thisvec = prob_irdc(s[i], constrain=constraint, ALGORITHM=2, $
                         MAKE_PS=make_ps, _EXTRA=_extra, FMEAS=send, $
                         MDIST=send2, ALPHA=alpha, SILENT=silent)
     
     IF (s[i].cnum MOD 50) EQ 0 && ~silent THEN $
        print,'Working BGPS #'+string(s[i].cnum,format="(I4)")
     
     jj = WHERE(pvec.cnum EQ s[i].cnum, njj)
     IF njj EQ 0 THEN message,'Danger, Will Robinson!'
     
     IF save THEN BEGIN
        pvec[jj].invx = thisvec
        pvec[jj].has  = 1b
     ENDIF
     
     undefine,thisvec
  ENDFOR
  
  savefn = './emaf/save_files/irdc_morph_pvec'+suff_alpha+'.sav'
  
  IF save THEN save,pvec,filename=savefn,VERBOSE=~silent
  undefine,pvec
END
