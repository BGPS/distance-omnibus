;+
; NAME:
;       GENERATE_HI_SPECTRA_LOCAL
;
; PURPOSE:
;       Extracts HI spectra for all BGPS sources using Erik's
;       "on-off" method first seen in grsmatch.pro.  By extracting a
;       local set of spectra, this dramatically improves compute time
;       for prob_hisa.pro. 
;
; CATEGORY:
;       distance-omnibus
;
; CALLING SEQUENCE:
;       GENERATE_HI_SPECTRA_LOCAL [,bgps][,START=start][,REAR=rear]
;                                 [,CNUM_LIST=cnum_list][,RAD=r][,/PLOT]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       BGPS      -- Name of the BGPS Distance Database CSV file.
;                    (Default: bgps_distance_database.csv)
;       RAD       -- Rind radius, in BGPS pixels, for averaging
;                    the "off spectrum".  [Default: 12]
;
;       *Note: (CNUM_LIST) and (START, REAR) are mutually exclusive
;              options: if CNUM_LIST is supplied, then START & REAR
;              are ignored. 
;       CNUM_LIST -- List of BGPS catalog numbers for objects to
;                    validate (as opposed to running the entire
;                    catalog).  (Also, sets /PLOT keyword.)
;       START     -- First BGPS catalog number to process
;                    [Default: #1]
;       REAR      -- Last BGPS catalog number to process [Default:
;                    last entry in the catalog]
;
; KEYWORD PARAMETERS:
;       PLOT      -- Generate plots
;
; OUTPUTS:
;       Creates the IDL save file: ./local/bgps_hi_spectra.sav  (~232 MB)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;       Created:  08/10/10, TPEB -- Initial version, swiped from
;                                   generare_grs_spectra_local.pro.
;       Modified: 10/17/10, TPEB -- Added input for alternate "rind"
;                                   radii for the OFF spectrum
;                                   extraction.
;       Modified: 04/27/11, TPEB -- Documentation and code cleanup.
;       Modified: 04/29/11, TPEB -- Undefine the structure hi at the
;                                   end of the code for memory-saving
;                                   reasons.
;       Modified: 09/12/11, TPEB -- Updated START/REAR/CNUM_LIST to
;                                   conform in usage with other
;                                   routines in the distance-omnibus
;                                   project.
;       Modified: 12/09/11, TPEB -- Fixed bug in addition of REAR
;                                   keyword (IDL sees R as ambiguous
;                                   with REAR) by changing R -> RAD
;
;-

PRO GENERATE_HI_SPECTRA_LOCAL, bgps, START=start, REAR=rear, $
                               CNUM_LIST=cnum_list, PLOT=plot, RAD=r
  
  COMMON HISA_BLOCK,hi_data,hd,lastfn,cont_data,c_hd,have_cont
  
  lastfn = ''
  
  ;; Get galactic params
  defsysv, '!MW', exists = exists
  IF NOT exists THEN galactic_params 
  
  ;; Set default BGPS CSV file, if not specified
  IF n_elements(bgps) EQ 0 THEN bgps = 'bgps_distance_database.csv'
  IF n_elements(rad) EQ 0 THEN r = 12.
  
  nosave = KEYWORD_SET(start) OR KEYWORD_SET(rear) OR KEYWORD_SET(cnum_list) $
           OR KEYWORD_SET(plot)
  
  s = read_bgps_csv(bgps,csv,/ver)
  restore,'local/bgps_v102_map_locations.sav',/verbose
  postage = readfits('./local/postage/bgps0001.fits',hdr,/silent)
  radius = 0.5 * (size(postage,/DIM)-1)[0]
  
  hi = REPLICATE( $
       CREATE_STRUCT('cnum',long(0),'spectrum',fltarr(!MW.NVBINS),$
                     'v_std',fltarr(!MW.NVBINS),$
                     'onspec',fltarr(!MW.NVBINS),$
                     'bdrspec',fltarr(!MW.NVBINS),$
                     'border',bytarr(size(postage,/DIM)), 'R', 0.),$
       csv.nrows)
  
  
  ;; If CNUM_LIST supplied, then only do the big loop for those objects
  IF n_elements( cnum_list ) NE 0 THEN BEGIN
     n_bgps = n_elements( cnum_list ) 
     ind = WHERE_ARRAY(cnum_list, s.cnum)
     s = s[ind]
     start = 0L
     rear = n_bgps-1
  ENDIF ELSE BEGIN
     ;; Else, look for START & REAR keywords and adjust accordingly
     n_bgps = csv.nrows
     IF n_elements(start) NE 0 THEN start = long(start) ELSE start = 0L
     IF n_elements(rear) NE 0 THEN rear = long(rear) ELSE rear = n_bgps-1
  ENDELSE  
  
  IF KEYWORD_SET(plot) THEN window,1
  
  FOR i=start, n_bgps-1 DO BEGIN
     
     hi[i].cnum = s[i].cnum
     message,string(s[i].cnum,format="('Now doing BGPS #',I4)"),/inf
     
     hi[i].spectrum = hisa(s[i], v_std = v_std, onspec=onspec,$
                           bdrspec=bdrspec, PLOT=plot, border=border, $
                           bhdr=bhdr, R=r)
     hi[i].v_std   = v_std
     hi[i].onspec  = onspec
     hi[i].bdrspec = bdrspec
     hi[i].r       = r
     
     IF TOTAL(hi[i].spectrum NE 0) THEN BEGIN
        
        ;; Trim border down to postage stamp size
        start = [bgps[i].xpos-radius,bgps[i].ypos-radius]
        subsize = [radius,radius]*2+1
        
        ;; As part of this... need to associate WCS with new subsection...
        ;; Check starting and stopping pixels to ensure in range...
        sz = size(border,/DIM)
        ind = WHERE(start LT 0, nind)
        IF nind NE 0 THEN start[ind] = 0
        ind = WHERE(start+subsize GT sz, nind)
        IF nind NE 0 THEN BEGIN
           excess = ((start + subsize) - sz)>0
           subsize -= excess
        ENDIF
        
        hextract,border,bhdr,ps_bdr,pshd,start[0],start[0]+subsize[0]-1,$
                 start[1],start[1]+subsize[1]-1,/silent
        
        hi[i].border  = ps_bdr
     ENDIF
     
     IF KEYWORD_SET(cnum_list) THEN wait,5
     
  ENDFOR
  
  IF ~ nosave THEN BEGIN
     save,hi,filename='./local/bgps_hi_spectra_r'+string(r,format="(I0)")+$
          '.sav',/ver
     message,'END Time: ',/inf
     spawn,'date'
  ENDIF
  
  ;; For memory-saving purposes, undefine hi before exiting.
  undefine, hi
  
END
