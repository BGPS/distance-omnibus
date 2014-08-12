;+
; NAME:
;       OMNI_GRSMATCH
;
; PURPOSE:
;       Generate a spectrum for sources in the GRS overlap by
;       integrating over the source and subracting off a boundary spectrum.
;
; CATEGORY:
;       distance-omnibus

; CALLING SEQUENCE:
;       spectrum = OMNI_GRSMATCH( struct [,V_STD=v_std][,ONSPEC=onspec]
;                           [,BDRSPEC=bdrspec][,OBJMASK=objmask]
;                           [,BORDER=border][,X_ORIG=x_orig][,Y_ORIG=y_orig]
;                           [,X_BDR=x_bdr][,Y_BDR=y_bdr][,BHDR=bhdr][,R=r] )
;
; INPUTS:
;       STRUCT   -- Single element of the survey catalog structure, as
;                   read in by OMNI_READ_CAT().
;
; OPTIONAL INPUTS:
;       R        -- [arcsec] Rind radius for averaging the "off
;                   spectrum".  [Default: ancil.grs_rad]
;
; KEYWORD PARAMETERS:
;       NOFILTER -- DO not apply a SAVGOL filter to the GRS data.
;
; OUTPUTS:
;       SPECTRUM -- Integrated masked spectrum for the BGPS source
;
; OPTIONAL OUTPUTS:
;       V_STD    -- Velocity vector associated with the SPECTRUM
;       ONSPEC   -- "On-Source" HI spectrum weighted by BGPS emission.
;       BDRSPEC  -- "Border" HI spectrum, averaged over admissible
;                   border pixels
;       OBJMASK  -- BGPS catalog object mask -- pixels from BGPS
;                   assigned to this object.
;       BORDER   -- "Rind" around the BGPS source, excluding pixels
;                   assigned to other catalog objects.
;       X_ORIG   -- X-axis pixel numbers for the BGPS source
;       Y_ORIG   -- Y-axis pixel numbers for the BGPS source
;       X_BDR    -- X-axis pixel numbers for the border region
;       Y_BDR    -- Y-axis pixel numbers for the border region
;       BHDR     -- BGPS image header
;       STATUS   -- 0: no spectrum, 
;                   1: good spectrum, 
;                   4: landlocked -- no spectrum (See OMNI_GRS_MASKSPEC)
;
; COMMON BLOCKS:
;       OMNI_CONFIG    -- The set of configuration structures, read in
;                         from the config files in conffiles/
;       GRSMATCH_BLOCK -- Block containing GRS data for looping.
;
; MODIFICATION HISTORY:
;       Fri Jun 11 13:36:03 2010, erosolo <erosolo@>
;                 Documented
;       Thu Jul 22 11:14:30 2010, erosolo <erosolo@>
;		Updated to newest version.
;       Modified: 08/03/10, TPEB -- Extracted the spectrum creation
;                                   part of the original
;                                   prob_grsmatch.pro routine.
;       Modified: 06/15/11, TPEB -- Replaced DELVARX with UNDEFINE as the
;                                   means for clearing the data cubes
;                                   from memory before loading the next
;                                   one.  Fixed memory leak & improved
;                                   reliability.  Also, cleaned up
;                                   documentation.
;       Modified: 08/13/13, TPEB -- Move to OMNI_* framework.  Also a
;                                   speedup.  Removed many unneeded
;                                   keyword parameters.  Updated
;                                   documentation.
;       Modified: 08/20/13, TPEB -- Cleaned up the GRS_BLOCK a little,
;                                   removing unused items and
;                                   consolidating small items into an
;                                   IDL structure.
;       Modified: 12/18/13, TPEB -- Added means for returning ONSPEC
;                                   and flag for LANDLOCKED cases.
;       Modified: 12/23/13, TPEB -- Added optional flag for NOFILTER
;                                   to not SvaGol-filter the spectra.
;                                   To compensate, adjust NaN flags in
;                                   routine calls and set
;                                   out-of-bounds velocities to zero
;                                   rather than NaN.
;       Modified: 01/20/14, TPEB -- Actually return ONSPEC for
;                                   LANDLOCKED sources (bug fix).
;       Modified: 08/12/14, TPEB -- Fixed bug involving object not in
;                                   the label mask.  Now skips the
;                                   object.
;
;-

FUNCTION OMNI_GRSMATCH, s, V_STD=v_std, onspec=onspec, bdrspec=bdrspec, $
                        objmask=objmask, border=border, bhdr=bhdr, R=rad, $
                        x_orig=x_orig, y_orig=y_orig, x_bdr=x_bdr, y_bdr=y_bdr,$
                        STATUS=status, NOFILTER=nofilter
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON GRSMATCH_BLOCK, grsblock, grs_data, hd, s_data, obj, survey, $
     survey_maps, survey_label, grs_fn, bastrom, astrom, h, outind
  
  ;; Define COMMON structure to hold the small items, 
  ;;    if not already defined 
  IF ~n_elements(grsblock) THEN $
     grsblock = {lastfn:'',$    ; Last GRS cube filename loaded into grs_data
                 s_fn:'',$      ; Last SURVEY image filename loaded into s_data
                 pixscale:0.d,$ ; Pixel scale of the SURVEY images
                 sz_grs:lonarr(6),$ ; SIZE array for the current GRS cube
                 sz:lonarr(5),$     ; SIZE array for the current SURVEY image
                 filter:savgol(17,17,0,6)} ; SAVGOL filter
  
  ;; Set false status, change to true at end
  status = 0b
  nofilter = KEYWORD_SET(nofilter)
  
  ;; Read in the configuration files
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN,0.
  ENDIF
  IF ~conf.haslabel THEN BEGIN
     message,'This survey does not have label maps... cannot compute GRS '+$
             'On-Off spectra.  Kindly go jump in a lake.',/cont
     RETURN,0.
  ENDIF
  IF ~exist(mw) THEN $
     mw = omni_read_conffile('./conffiles/galactic_params.conf')
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN,0.
  ENDIF
  IF ~exist(ancil) THEN $
     ancil = omni_read_conffile('./conffiles/ancillary.conf')
  
  
  ;; Check to see if the MAP_LIST and GRS_LIST files are in GRS_BLOCK
  IF ~n_elements(survey_maps) THEN $
     readcol,conf.maps,survey_maps,format='a',count=n_map,/SILENT,comment='#'
  IF ~n_elements(survey_label) THEN $
     readcol,conf.label,survey_label,format='a',count=n_lab,/SILENT,comment='#'
  IF ~n_elements(grs_fn) THEN $
     readcol,local.grs,grs_fn,format='a',count=n_grs,/SILENT,comment='#'
  
  ;; Check to see if the SURVEY structure has already been loaded
  IF ~n_elements(survey) THEN BEGIN
     ;; Load in the map locations save file
     mlfn = './local/'+conf.survey+'_map_locations.sav'
     IF ~FILE_TEST(mlfn,/READ) THEN omni_assoc_catalog, CONFFILE=cfile
     restore, mlfn, /ver
  ENDIF
  
  ;;=====================================================================
  ;; Set up the arrays needed
  IF ~n_elements(v_std) THEN $
     v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
  null_spec = v_std * 0.
  
  
  ;; Check to see if this source lies within the GRS coverage
  IF s.glon LT 14 OR s.glon GT 60 THEN BEGIN
     message,conf.survey+' source lies outside GRS coverage...',/inf
     RETURN, null_spec
  ENDIF
  
  
  ;; Find the element in the SURVEY structure that corresponds to the
  ;;   object passed to this routine.
  hit = where(survey.cnum EQ s.cnum, ct) 
  IF ~ct THEN BEGIN
     message,'No match between catalog and map locations save file...  '+$
             'Either re-run omni_assoc_catalog or start drinking heavily.',/cont
     RETURN,null_spec
  ENDIF
  
  
  ;;=====================================================================
  ;; Convert the r in arcsec into SURVEY pixels
  IF ~n_elements(rad) THEN rad = ancil.grs_rad
  
  ;; Check for pixscale, compute if not present in the COMMON structure
  IF grsblock.pixscale EQ 0.d THEN BEGIN
     ;; Use the header from the first data image in the conf.maps to
     ;;   compute the required radius for the rind limits.
     hdr = headfits(survey_maps[0])
     extast,hdr,astr,ch_par
     CASE ch_par OF             ; See documentation for EXTAST
        1: grsblock.pixscale = abs(astr.cdelt[0])*3600.
        3: grsblock.pixscale = abs(astr.cdelt[0])*3600.
        2: grsblock.pixscale = abs(astr.cd[0,0]) * 3600.
        ELSE: message,'Error: Astrometry information for '+$
                      survey[i].mapname+' is not useable!'
     ENDCASE
  ENDIF
  
  ;; Make rind mask
  r = float(round(ancil.grs_rad / grsblock.pixscale))
  elt = shift(dist(2*r+1,2*r+1),r,r) LE r
  ;;=====================================================================
  
  
  
  
  ;;========================================================================
  ;; The array element for this survey object is called this.  
  ;;  'j' is the element in survey_maps corresponding to this object.
  this = survey[hit]
  j = where(strmatch(survey_maps,'*'+this.mapname,/fold_case))
  
  
  ;; Check to see if we have this SURVEY data and label map already
  ;;   loaded into memory.
  IF this.mapname NE grsblock.s_fn THEN BEGIN
     s_data = readfits(survey_maps[j],bhdr,/SILENT)
     obj  = readfits(survey_label[j],lhdr,/SILENT)
     
     extast, bhdr, bastrom
     grsblock.s_fn = this.mapname ; Set up for next loop
  ENDIF
  grsblock.sz = size(s_data)
  
  
  ;; Check to see where this SURVEY object lands in the GRS data
  objmask = obj EQ this.labval
  ind = where(objmask, ct)
  IF ~ct || this.labval EQ 0 THEN BEGIN
     message,'No match in SURVEY label map...',/cont
     RETURN, null_spec
  ENDIF
  
  
  ;;=================================================================
  ;; Create the weights from the map intensity, and create the rind
  ;;   region.  This is all Erik's handiwork...
  wts = s_data[ind]
  x_orig = ind MOD grsblock.sz[1]
  y_orig = ind / grsblock.sz[1]
  
  border = (dilate(objmask, elt)-objmask)*(obj eq 0)
  ind2 = where(border,ctbdr)
  x_bdr = ind2 MOD grsblock.sz[1]
  y_bdr = ind2 / grsblock.sz[1]
  
  xy2ad, x_orig, y_orig, bastrom, glon, glat
  xy2ad, x_bdr, y_bdr, bastrom, glonbdr, glatbdr
  
  ;; Get the filenames for the ONSPEC
  wtorder = sort(glon)
  glon = glon[wtorder]
  glat = glat[wtorder]
  wts = wts[wtorder]
  filename = '*grs-'+strtrim(string(round(median(glon))), 2)+'-cube.fits'
  fn = grs_fn[where(strmatch(grs_fn,filename),nfn)]
  
  ;; Get the filenames for the BDRSPEC
  wt2order = sort(glonbdr)
  glonbdr = glonbdr[wt2order]
  glatbdr = glatbdr[wt2order]
  filename2 = '*grs-'+strtrim(string(round(median(glonbdr))), 2)+'-cube.fits'
  fn2 = grs_fn[where(strmatch(grs_fn,filename2),nfn2)]
  
  ;; Error checking
  IF nfn EQ 0 || nfn2 EQ 0 THEN BEGIN
     message,conf.survey+' source lies outside GRS coverage...',/inf
     RETURN, null_spec
  ENDIF
  
  
  ;; Create blank spectra for adding to, pixel by pixel
  runspec = null_spec
  
  ;; Loop through all SURVEY pixels assigned to this source
  FOR jj=0, n_elements(ind)-1 DO BEGIN
     IF ~FILE_TEST(fn,/READ) THEN CONTINUE
     
     ;; Pick GRS filename and load a new one if necessary
     IF fn NE grsblock.lastfn THEN BEGIN
        UNDEFINE,grs_data
        print,fn
        grs_data = readfits(fn, hd)
        grsblock.lastfn = fn
        extast, hd, astrom
        rdhd, hd, s = h
        grsblock.sz_grs = size(grs_data)
        outind = where(v_std LT min(h.v) OR v_std GT max(h.v))
     ENDIF
     ad2xy, glon[jj], glat[jj], astrom, x, y ; New ad2xy spits arrays
     x = x[0]                                ; even if scalar went in
     y = y[0]                                ; Grr.
     
     ;; Look up spectrum
     IF x LT 0 || x GT grsblock.sz_grs[1]-1 || $
        y LT 0 || y GT grsblock.sz_grs[2]-1 THEN BEGIN
        wts[jj] = 0
        CONTINUE
     ENDIF
     spectrum = reform(grs_data[x,y,*],grsblock.sz_grs[3])
     
     ;; Interpolate to common velocity scale
     specout = interpol(spectrum, h.v, v_std, /NAN)
     specout[outind] = 0.       ; !values.f_nan -- set to 0 not NaN
     runspec = runspec+specout*wts[jj]
     
  ENDFOR   ;; End of loop for ON-SOURCE spectra extraction
  
  ;; Filter the spectra
  specout = runspec/total(wts)
  spectrum = ~nofilter ? convol(specout,grsblock.filter,/edge_trun) >0. : $
             specout > 0.
  onspec = spectrum
  
  ;; Check to see if a source is landlocked before computing BORDER
  ;;   spectrum -- we still want to return the ONSPEC
  IF ctbdr EQ 0 THEN BEGIN
     message,conf.survey+' source is landlocked.  Using ONSPEC instead...',/inf
     status = 4b              ; Landlocked flag -- aligned w/ OMNI_GRS_MASKSPEC
     RETURN,onspec            ; Return ONSPEC for these objects.
  ENDIF
  
  
  
  ;; Create blank spectra for adding to, pixel by pixel
  bdrspec = null_spec
  
  ;; Loop through all BORDER pixels
  FOR jj=0, n_elements(ind2)-1 DO BEGIN
     IF ~FILE_TEST(fn2,/READ) THEN CONTINUE
     
     ;; Pick GRS filename and load a new one if necessary
     IF fn2 NE grsblock.lastfn THEN BEGIN
        UNDEFINE,grs_data
        print,fn2
        grs_data = readfits(fn2, hd)
        grsblock.lastfn = fn2
        extast, hd, astrom
        rdhd, hd, s = h
        grsblock.sz_grs = size(grs_data)
        outind = where(v_std LT min(h.v) OR v_std GT max(h.v))
     ENDIF
     ad2xy, glonbdr[jj], glatbdr[jj], astrom, x, y ; New ad2xy spits arrays
     x = x[0]                                      ; even if scalar went in
     y = y[0]                                      ; Grr.
     
     
     ;; Look up spectrum
     IF x LT 0 || x GT grsblock.sz_grs[1]-1 || $
        y LT 0 || y GT grsblock.sz_grs[2]-1 THEN BEGIN
        ctbdr = ctbdr - 1
        CONTINUE
     ENDIF
     spectrum = reform(grs_data[x,y,*],grsblock.sz_grs[3])
     
     ;; Interpolate to common velocity scale
     specout = interpol(spectrum, h.v, v_std, /NAN)
     specout[outind] = 0.       ; !values.f_nan -- set to 0 not NaN
     bdrspec = bdrspec+specout
     
  ENDFOR   ;; End of loop for OFF-SOURCE spectra extraction
  
  ;; Filter the spectra
  bdrspec = bdrspec/ctbdr
  bdrspec  = ~nofilter ? convol(bdrspec,grsblock.filter,/edge_trun) >0. : $
             bdrspec > 0.
  
  
  ;; Create the Final Spectrum     
  emmask = onspec GT 4*mad(onspec)
  spectrum = ((onspec - bdrspec)>0)*emmask
  
  ;; Error check.
  IF total(spectrum) EQ 0 THEN BEGIN
     message,'No spectrum extracted for this source.',/inf
     RETURN,null_spec
  ENDIF
  
  status = 1b                   ; We have success!
  RETURN,spectrum
  
END
