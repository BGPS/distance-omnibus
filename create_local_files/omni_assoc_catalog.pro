;+
; NAME:
;       OMNI_ASSOC_CATALOG
;
; PURPOSE:
;       Creates an IDL save file in the local/ directory according to
;       the configuration file survey_info.conf.  The save file
;       associates each catalog entry with a position in the survey
;       release maps, and selects the appropriate mosaic image based
;       on a label map associated with the catalog.
;
; CATEGORY:
;       distance-omnibus Local File Creation
;
; CALLING SEQUENCE:
;       OMNI_ASSOC_CATALOG [,CONFFILE=cfile][,START=start]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;       START    -- Catalog entry number to start with [Default: first]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE (Creates an IDL save file in the local/ directory
;       according to the conffile.)
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
;       Created:  08/25/10, TPEB -- Initial (undocumented) version.
;       Modified: 07/14/11, TPEB -- Added documentation, code cleanup.
;       Modified: 07/19/11, TPEB -- Added check for correct PPBEAM
;                                   FITS keyword in the BGPS header.
;       Modified: 01/12/12, TPEB -- Additional comments and code
;                                   cleanup.
;       Modified: 07/23/12, TPEB -- Added smoothed BGPS map name to
;                                   the structure.
;       Modified: 02/19/13, TPEB -- NEW ROUTINE NAME, modified from
;                                   the BGPS-specific version to be
;                                   configurable with a conffile.
;       Modified: 02/21/13, TPEB -- Fully-functional version that
;                                   produces the same results as
;                                   assoc_bgps_bolocat.pro
;       Modified: 02/27/13, TPEB -- Added COMMENT='#' to the readcol
;                                   calls to allow for comments in the
;                                   filename list files.
;       Modified: 03/08/13, TPEB -- Fixed bug in extracting value from
;                                   label map -- set x & y positions
;                                   to type LONG.
;       Modified: 03/20/13, TPEB -- Added CONFFILE optional input for
;                                   conformity with other routines.
;       Modified: 06/05/13, TPEB -- Added NOISE structure element
;                                   containing the MEAN of the
;                                   noisemap within the label contour,
;                                   if both the LABEL and NOISE maps
;                                   are present in survey_info.conf.
;                                   Plus minor bug fixes.
;       Modified: 07/31/13, TPEB -- Added LABVAL structure element to
;                                   aid with velocity matching via
;                                   label maps.  Minor bug fixes.
;       Modified: 08/01/13, TPEB -- Include the conf structure in the
;                                   map_locations IDL save file as
;                                   survey_info to encode such
;                                   information as the catalog used to
;                                   create the SURVEY structure.
;
;-

PRO OMNI_ASSOC_CATALOG, CONFFILE=cfile, START=start
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  start = ~n_elements(start) ? 0L : long(start-1)
  
  ;; Read in the configuration file
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN
  ENDIF
  
  ;; Check information from the config file
  IF ~conf.hasmap THEN BEGIN
     message,'This survey has no map data. Exiting.',/cont
     RETURN
  ENDIF
  
  ;; Read in the survey catalog & count entries
  s = omni_read_cat(conf.cat,ncat,fmt)
  
  
  ;; Find all the survey data mosaics, label maps, and smoothed images
  readcol,conf.maps,survey_maps,format='a',count=n_map,/SILENT,comment='#'
  IF conf.haslabel THEN $
     readcol,conf.label,survey_label,format='a',count=n_label, $
             /SILENT,comment='#' ELSE n_label = 0
  IF conf.hassmooth THEN $
     readcol,conf.smooth,survey_smoo,format='a',count=n_smoo, $
             /SILENT,comment='#' ELSE n_smoo = 0
  IF conf.hasnoise THEN $
     readcol,conf.noise,survey_noise,format='a',count=n_noise, $
             /SILENT,comment='#' ELSE n_noise = 0
  IF conf.hascrop THEN $
     readcol,conf.crop,fieldname,lmin,lmax,bmin,bmax,format='a,f,f,f,f',$
             count=n_crop,/SILENT,comment='#' ELSE n_crop = 0
  
  message,$
     string(n_map,n_label,n_smoo,n_noise,n_crop,format=$
            "('Found -- N_MAP: ',I0,'  N_LABEL: ',I0,"+$
            "'  N_SMOO: ',I0,'  N_NOISE: ',I0,'  N_CROP: ',I0)"),/inf
  
  IF n_map EQ 0 THEN BEGIN
     message,'No map images found.  Exiting.',/cont
     RETURN
  ENDIF
  
  ;; Check that the number of found maps is correct, given the conffile
  IF (conf.haslabel  AND n_map NE n_label) || $
     (conf.hassmooth AND n_map NE n_smoo)  || $
     (conf.haslabel  AND conf.hassmooth AND n_label NE n_smoo) THEN BEGIN
     message,'Mismatch between maps, labels, and smoothed images!',/cont
     RETURN
  ENDIF
  
  ;; Make structure for holding image association locations
  struct = {cnum:0L,$
            mapname:'',$
            labelname:'',$
            smooname:'',$
            xpos:0L,$
            ypos:0L,$
            labval:0L,$
            glon:0.d,$
            glat:0.d}
  IF conf.hasnoise && conf.haslabel THEN $
     struct = create_struct(struct,'noise',0.d)
  survey = replicate(struct, ncat)
  
  ;; Fill in various structure elements
  survey.cnum = s.cnum
  survey.glon = s.glon
  survey.glat = s.glat
  q = '"'
  
  
  ;; For matching survey sources to map mosaics, read in the FITS
  ;;   headers of the maps, and populate a structure with GLON and
  ;;   GLAT ranges for each
  mapdata = replicate( {fn:'',$
                        naxis:[0,0],$
                        l:[0.d,0.d],$
                        b:[0.d,0.d]}, n_map)
  
  message,'Reading map data into memory...',/inf
  FOR j=0, n_map-1 DO BEGIN
     ;; Read in map data & label map (if available) for use later
     jst = string(j,format="(I0)")
     command = 'map'+jst+' = readfits(survey_maps[j],hdr,/SILENT)'
     errcode = Execute(command)
     IF conf.haslabel THEN BEGIN
        command = 'label'+jst+' = readfits(survey_label[j],lhd'+jst+',/SILENT)'
        errcode = Execute(command)
     ENDIF
     IF conf.hasnoise THEN BEGIN
        command = 'noise'+jst+' = readfits(survey_noise[j],nhd'+jst+',/SILENT)'
        errcode = Execute(command)
     ENDIF
     
     ;; Create an ASTR structure
     extast,hdr,astr
     
     mapdata[j].fn    = survey_maps[j]
     mapdata[j].naxis = astr.naxis
     
     ;; Check if we have CROP boundaries.  If so, use these in
     ;;   mapdata, else compute from the headers
     IF conf.hascrop THEN BEGIN
        
        mapdata[j].l = [lmin[j],lmax[j]]
        mapdata[j].b = [bmin[j],bmax[j]]
        
        ;; Check for tiles spanning l=0
        IF mapdata[j].l[1] - mapdata[j].l[0] GE 300 THEN BEGIN
           mapdata[j].l = [mapdata[j].l[1],mapdata[j].l[0]] ; Reverse
           mapdata[j].l[0] -= 360.                          ; Negative lower
        ENDIF           
        
     ENDIF ELSE BEGIN
        xval = (findgen(astr.naxis[0])-astr.crpix[0])*astr.cd[0,0]+astr.crval[0]
        yval = (findgen(astr.naxis[1])-astr.crpix[1])*astr.cd[1,1]+astr.crval[1]
        
        mapdata[j].l     = minmax(xval)
        mapdata[j].b     = minmax(yval)
        
     ENDELSE
     ;; Save ASTR structure for each map to be used later
     command = string(j,format="('astr',I0,' = astr')")
     errcode = Execute(command)
     
     ;; Keep memory clear
     undefine,xval,yval,hdr,astr
     
  ENDFOR
  
  
  ;;===================================================================
  ;; Loop through each survey source, and fill in the survey structure.
  message,'Looping through catalog sources...',/inf
  FOR i=start, ncat-1 DO BEGIN
     
     ;; Check this object's position against the mapdata
     ;;   structure, with cases for glon near 0 deg.
     hit = WHERE( s[i].glon GE mapdata.l[0] AND $
                  s[i].glon LE mapdata.l[1] AND $
                  s[i].glat GE mapdata.b[0] AND $
                  s[i].glat LE mapdata.b[1], nhit )
     s[i].glon -= 360.
     hitm = WHERE( s[i].glon GE mapdata.l[0] AND $
                   s[i].glon LE mapdata.l[1] AND $
                   s[i].glat GE mapdata.b[0] AND $
                   s[i].glat LE mapdata.b[1], nhitm )
     s[i].glon += 720.
     hitp = WHERE( s[i].glon GE mapdata.l[0] AND $
                   s[i].glon LE mapdata.l[1] AND $
                   s[i].glat GE mapdata.b[0] AND $
                   s[i].glat LE mapdata.b[1], nhitp )
     s[i].glon -= 360.
     
     ;; Check that we found something
     nmatch = nhit + nhitm + nhitp
     IF nmatch EQ 0 THEN BEGIN
        message,'Error: No map match for catalog #'+$
                string(s[i].cnum,format=fmt),/inf
        CONTINUE
     ENDIF
     
     ;; Re-aggregate the results
     hits = hit
     IF nhitm THEN hits = [hits,hitm]
     IF nhitp THEN hits = [hits,hitp]
     ;; Remove any "-1" entries from hits 
     hits = hits[missing([-1],hits)]
     
     
     ;;===================================================================
     ;; If nmatch = 1, we are good to go, but we need to figure out
     ;;   which image should be used for objects with positions in
     ;;   more than one map.
     
     IF nmatch NE 1 THEN BEGIN
        ;; This process is facilitated if label maps are provided
        IF conf.haslabel THEN BEGIN
           
           bestj = -1
           ;; Loop through the label maps, run adxy, check for non-zero
           ;;   value at location of peak flux density.
           FOR ll=0,nmatch-1 DO BEGIN
              j = hits[ll]
              jst = string(j,format="(I0)")
              command = 'adxy,lhd'+jst+',s[i].glon,s[i].glat,x,y'
              errcode = Execute(command)
              x = long(round(x))
              y = long(round(y))
              IF( x LT 0 || x GE mapdata[j].naxis[0] ) THEN CONTINUE
              IF( y LT 0 || y GE mapdata[j].naxis[1] ) THEN CONTINUE
              command = 'val = label'+jst+'[x,y]'
              errcode = Execute(command)
              IF errcode NE 1 THEN STOP
              IF val NE 0 THEN bestj = j
           ENDFOR  
           j = bestj
        ENDIF ELSE BEGIN
           ;; Else, this is a little messier
           
           ;; Loop through the data maps, run ad2xy, check for finite
           ;;   data value at the location of peak flux density
           bestj = -1
           bestx = 0
           FOR ll=0,nmatch-1 DO BEGIN
              j = hits[ll]
              jst = string(j,format="(I0)")
              command = 'ad2xy,s[i].glon,s[i].glat,astr'+jst+',x,y'
              errcode = Execute(command)
              IF( x LT 0 || x GE mapdata[j].naxis[0] ) THEN CONTINUE
              
              ;; Check that there are no NaNs within the vicinity of
              ;;   this location in the map  (5x5 box)
              xb = ((long(round(x)) + [-2,2]) > 0) < (mapdata[j].naxis[0]-1)
              yb = ((long(round(y)) + [-2,2]) > 0) < (mapdata[j].naxis[1]-1)
              command = 'val = map'+jst+'[xb[0]:xb[1],yb[0]:yb[1]]'
              errcode = Execute(command)
              IF fix(total(~finite(val))) THEN CONTINUE
              
              ;; Select the map for which this point lies farthest from
              ;;   the edge.  ----- NOT RIGHT METRIC!!!1!
              xedge = long(round(abs(x) < abs(mapdata[j].naxis[0]-x-1)))
              IF xedge GT bestx THEN BEGIN
                 bestx = xedge
                 bestj = j
              ENDIF
           ENDFOR
           j = bestj
        ENDELSE
     ENDIF ELSE j = hits        ; End of the nmatch > 1 checking section
     ;;===================================================================
     
     
     
     ;;=======================================================
     ;; Get the survey image mapname, and place in structure
     ;; Get the peak position of the survey source in the image
     jst = string(j,format="(I0)")
     command = 'ad2xy,s[i].glon,s[i].glat,astr'+jst+',x,y'
     errcode = Execute(command)
     
     x = (long(round(x)))[0] ; SCALAR!
     y = (long(round(y)))[0] ; SCALAR!
     
     IF( x LT 0 || x GE mapdata[j].naxis[0] ) THEN BEGIN
        print,'Catalog #'+string(survey[i].cnum,format=fmt)+$
              '  WARNING!!!  XPOS: '+string(x,format="(I0)")
     ENDIF ELSE BEGIN
        survey[i].mapname  = $
           strmid(survey_maps[j],strpos(survey_maps[j],'/',/REVERSE_SEARCH)+1)
        IF conf.haslabel THEN $
           survey[i].labelname = $
           strmid(survey_label[j],strpos(survey_label[j],'/',/REVERSE_SEARCH)+1)
        IF conf.hassmooth THEN $
           survey[i].smooname = $
           strmid(survey_smoo[j],strpos(survey_smoo[j],'/',/REVERSE_SEARCH)+1)
        survey[i].xpos = x
        survey[i].ypos = y
        
        command = 'survey[i].labval = label'+jst+'[x,y]'
        errcode = Execute(command)
        
        ;; Compute mean of noise map within label contour
        IF conf.hasnoise && conf.haslabel THEN BEGIN
           command = 'ind = where(label'+jst+' EQ survey[i].labval,nxy)'
           errcode = Execute(command)
           command = 'survey[i].noise = mean(noise'+jst+'[ind])'
           errcode = Execute(command)
        ENDIF
     ENDELSE
     
     ;; IF count NE 1 THEN $
     ;;    message,string(survey[i].cnum,count,format=$
     ;;                   "('Catalog #',I4,' has ',I0,' matches!')"),/inf
  ENDFOR
  
  ;; Save all this to disk for later joy
  survey_info = conf
  save,survey,survey_info,/verbose,$
       filename='./local/'+conf.survey+'_map_locations.sav'
  
  ;; Clean up the memory
  undefine,survey
  
END
