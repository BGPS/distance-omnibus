;+
; NAME:
;       OMNI_GRS_SPECTRA_LOCAL
;
; PURPOSE:
;       Extracts GRS spectra for all BGPS sources using Erik's
;       "on-off" method in grsmatch.pro.  By extracting a local set of
;       spectra, this dramatically improves compute time for
;       prob_grsmatch.pro.
;
; CATEGORY:
;       distance-omnibus
;
; CALLING SEQUENCE:
;       OMNI_GRS_SPECTRA_LOCAL [,CONFFILE=cfile] [,START=start]
;                              [,REAR=rear] [,RAD=rad]  
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;       START    -- Catalog entry number to start with [Default: first]
;       REAR     -- Catalog entry number to end with [Default: last]
;
; KEYWORD PARAMETERS:
;       NOFILTER -- Do not apply a SavGol filter to the data.
;
; OUTPUTS:
;       Creates the IDL save file: ./local/bgps_grs_spectra.sav  (~32 MB)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG    -- The set of configuration structures, read in
;                         from the config files in conffiles/
;       GRSMATCH_BLOCK -- Block containing GRS data for looping.
;
; MODIFICATION HISTORY:
;       Created:  08/04/10, TPEB -- Initial version.
;       Modified: 10/17/10, TPEB -- Added input for alternate "rind"
;                                   radii for the OFF spectrum extraction.
;       Modified: 12/09/11, TPEB -- Fixed bug in addition of REAR
;                                   keyword (IDL sees R as ambiguous
;                                   with REAR) by changing R -> RAD
;       Modified: 08/12/13, TPEB -- Moved to the OMNI_* framework,
;                                   including adding configuration
;                                   files and whatnot.
;       Modified: 08/20/13, TPEB -- Cleaned up the GRS_BLOCK a little,
;                                   removing unused items and
;                                   consolidating small items into an
;                                   IDL structure.
;       Modified: 12/18/13, TPEB -- Added structure element to record
;                                   whether a source is LANDLOCKED
;                                   (grs.flag = 2).
;       Modified: 12/23/13, TPEB -- Added NOFILTER keyword to not
;                                   apply a SavGol filter to the
;                                   data.
;       Modified: 01/28/14, TPEB -- Added extra screen output for
;                                   debugging.
;
;-

PRO OMNI_GRS_SPECTRA_LOCAL, CONFFILE=cfile, START=start, REAR=rear, RAD=r, $
                            NOFILTER=nofilter
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON GRSMATCH_BLOCK, grsblock, grs_data, hd, s_data, obj, survey, $
     survey_maps, survey_label, grs_fn, bastrom, astrom, h, outind
  
  ;; Parse keywords
  nofilter = KEYWORD_SET(nofilter)
  
  ;; Define COMMON structure to hold the small items, 
  ;;    if not already defined 
  IF ~n_elements(grsblock) THEN $
     grsblock = {lastfn:'',$    ; Last GRS cube filename loaded into grs_data
                 s_fn:'',$      ; Last SURVEY image filename loaded into s_data
                 pixscale:0.d,$ ; Pixel scale of the SURVEY images
                 sz_grs:lonarr(6),$ ; SIZE array for the current GRS cube
                 sz:lonarr(5),$     ; SIZE array for the current SURVEY image
                 filter:savgol(17,17,0,6)} ; SAVGOL filter
  
  ;; Read in the configuration files
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN
  ENDIF
  IF ~conf.haslabel THEN BEGIN
     message,'This survey does not have label maps... cannot compute GRS '+$
             'On-Off spectra.  Kindly go jump in a lake.',/cont
     RETURN
  ENDIF
  
  mw    = omni_read_conffile('./conffiles/galactic_params.conf')
  ancil = omni_read_conffile('./conffiles/ancillary.conf')
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN
  ENDIF
  
  ;; Read in the survey catalog & count entries
  s = omni_read_cat(conf.cat,ncat,fmt)
  fmt2 = string(ceil(alog10(ncat+1)),format="('I',I0)")
  
  ;; Parse keywords
  start = ~n_elements(start)  ? 0L            : long(start-1)
  rear  = ~n_elements(rear)   ? ncat-1        : long(rear-1)
  r     = ~n_elements(r)      ? ancil.grs_rad : double(r)
  size  = (local.pssize EQ 0) ? 8.5           : local.pssize
  
  ;; Set the lastfn variables as blank strings
  grsblock.lastfn = ''
  grsblock.s_fn   = ''
  ;; Read MAP_LIST and GRS_LIST files into GRS_BLOCK
  readcol,conf.maps,survey_maps,format='a',count=n_map,/SILENT,comment='#'
  readcol,conf.label,survey_label,format='a',count=n_label,/SILENT,comment='#'
  readcol,local.grs,grs_fn,format='a',count=n_grs,/SILENT,comment='#'
  
  
  ;; Load in the map locations save file
  mlfn = './local/'+conf.survey+'_map_locations.sav'
  IF ~FILE_TEST(mlfn,/READ) THEN omni_assoc_catalog, CONFFILE=cfile
  restore, mlfn, /ver
  
  
  ;; Use the header from the first data image in the conf.maps to
  ;;   compute the required radius for the rind limits.
  readcol,conf.maps,survey_maps,format='a',count=n_map,/SILENT,comment='#'
  hdr = headfits(survey_maps[0])
  extast,hdr,astr,ch_par
  CASE ch_par OF                ; See documentation for EXTAST
     1: grsblock.pixscale = abs(astr.cdelt[0])*3600.
     3: grsblock.pixscale = abs(astr.cdelt[0])*3600.
     2: grsblock.pixscale = abs(astr.cd[0,0]) * 3600.
     ELSE: message,'Error: Astrometry information for '+$
                   survey[i].mapname+' is not useable!'
  ENDCASE
  radius = long(round(size * 60. / grsblock.pixscale / 2.))
  
  
  ;;=================================================================
  ;; Create the structure and get ready to loop
  
  grs = replicate( {cnum:0L,$                              ; Catalog Number
                    l:0.d,$                                ; Galactic Longitude
                    b:0.d,$                                ; Galactic Latitude
                    spectrum:fltarr(conf.nvbin),$          ; On-Off Spectrum
                    onspec:fltarr(conf.nvbin),$            ; On Spectrum
                    bdrspec:fltarr(conf.nvbin),$           ; Off Spectrum
                    border:bytarr(2*radius+1,2*radius+1),$ ; Border Mask
                    flag:0b,$                              ; 0=n, 1=y, 2=ll
                    R:0.}, ncat)                           ; Border radius (")
                                ; Note: v_std will be computed from
                                ; survey_info configuration file and
                                ; not be included in this structure.
  v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
  
  
  FOR i=start, rear DO BEGIN
     
     ;; Load up structure elements
     grs[i].cnum = s[i].cnum
     grs[i].l    = s[i].glon
     grs[i].b    = s[i].glat
     message,'Working '+conf.survey+' object #'+$
             string(s[i].cnum,format='('+fmt2+')'),/inf
     
     
     ;; Run GRSMATCH, populate structure with outputs
     grs[i].spectrum = omni_grsmatch(s[i], v_std = v_std, onspec=onspec,$
                                     bdrspec=bdrspec, border=border, $
                                     bhdr=bhdr, R=r, STATUS=status, $
                                     NOFILTER=nofilter)
     grs[i].r    = r
     grs[i].flag = status
     SWITCH grs[i].flag OF      ; Use a SWITCH statement for maximum coolness.
        1: grs[i].bdrspec = bdrspec ; If good spectrum, record border and onspec
        4: grs[i].onspec  = onspec  ; If landlocked, only record onspec
        0: BREAK                    ; Else, don't do a damn thing.
        ELSE: message,'We have a serious problem if this line runs, rummy.'
     ENDSWITCH
     
     
     IF TOTAL(grs[i].spectrum NE 0) THEN BEGIN
        ;; Trim border down to postage stamp size
        start = [survey[i].xpos-radius,survey[i].ypos-radius]
        subsize = [radius,radius]*2+1
        ;; As part of this... need to associate WCS with new subsection...
        ;; Check starting and stopping pixels to ensure in range...
        bdrsz = size(border,/DIM)
        ind = WHERE(start LT 0, nind)
        IF nind NE 0 THEN start[ind] = 0
        ind = WHERE(start+subsize GT bdrsz, nind)
        IF nind NE 0 THEN BEGIN
           excess = ((start + subsize) - bdrsz)>0
           subsize -= excess
        ENDIF
        hextract,border,bhdr,ps_bdr,pshd,start[0],start[0]+subsize[0]-1,$
                 start[1],start[1]+subsize[1]-1,/silent
        
        grs[i].border = ps_bdr
     ENDIF
     
  ENDFOR
  
  fn_suf = ~nofilter ? '' : '_nofilt'
  
  save,grs,filename='local/'+conf.survey+'_grs_spectra_r'+$
       string(r,format="(I0)")+fn_suf+'.sav',/ver
  message,'FILENAME: '+'local/'+conf.survey+'_grs_spectra_r'+$
       string(r,format="(I0)")+fn_suf+'.sav',/inf
  
END
