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
;       Modified: 06/30/14, TPEB -- Add check for appropriate IDL
;                                   version.
;       Modified: 06/30/14, TPEB -- Convert storage of map information
;                                   from individual named variables
;                                   into lists (requires IDL 8.0+).
;                                   Also, tabulate number of pixels in
;                                   each source, and the peak flux
;                                   density pixel value.
;       Modified: 07/29/14, TPEB -- Make an error message more helpful.
;
;-

PRO OMNI_ASSOC_CATALOG, CONFFILE=cfile, START=start
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  omni_check_version            ; Check for an appropriate IDL version
  
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
            glat:0.d,$
            npix:0L,$
            maxflux:0.d}
  IF conf.hasnoise && conf.haslabel THEN $
     struct = create_struct(struct,'noise',0.d)
  survey = replicate(struct, ncat)
  
  ;; Fill in various structure elements
  survey.cnum = s.cnum
  survey.glon = s.glon
  survey.glat = s.glat
  
  ;; For matching survey sources to map mosaics, read in the FITS
  ;;   headers of the maps, and populate a structure with GLON and
  ;;   GLAT ranges for each
  mapdata = replicate( {fn:'',$
                        naxis:[0,0],$
                        l:[0.d,0.d],$
                        b:[0.d,0.d]}, n_map)
  
  message,'Reading map data into memory...',/inf
  
  ;; Create empty lists to contain the appropriate information
  maps   = list(!null)
  labels = list(!null)
  noises = list(!null)
  astrs  = list(!null)
  lhds   = list(!null)
  nhds   = list(!null)
  
  ;; Loop though files
  FOR jj=0, n_map-1 DO BEGIN
     
     ;; Read in map data & label map (if available) for use later
     maps.add, readfits(survey_maps[jj],hdr,/SILENT), jj
     
     IF conf.haslabel THEN BEGIN
        labels.add, readfits(survey_label[jj],lhd,/SILENT), jj
        lhds.add, lhd, jj
     ENDIF
     
     IF conf.hasnoise THEN BEGIN
        noises.add, readfits(survey_noise[jj],nhd,/SILENT), jj
        nhds.add, nhd, jj
     ENDIF
     
     ;; Create an ASTR structure
     extast,hdr,astr
     
     mapdata[jj].fn    = survey_maps[jj]
     mapdata[jj].naxis = astr.naxis
     
     ;; Check if we have CROP boundaries.  If so, use these in
     ;;   mapdata, else compute from the headers
     IF conf.hascrop THEN BEGIN
        
        mapdata[jj].l = [lmin[jj],lmax[jj]]
        mapdata[jj].b = [bmin[jj],bmax[jj]]
        
        ;; Check for tiles spanning l=0
        IF mapdata[jj].l[1] - mapdata[jj].l[0] GE 300 THEN BEGIN
           mapdata[jj].l = [mapdata[jj].l[1],mapdata[jj].l[0]] ; Reverse
           mapdata[jj].l[0] -= 360.                            ; Negative lower
        ENDIF           
        
     ENDIF ELSE BEGIN
        xval = (findgen(astr.naxis[0])-astr.crpix[0])*astr.cd[0,0]+astr.crval[0]
        yval = (findgen(astr.naxis[1])-astr.crpix[1])*astr.cd[1,1]+astr.crval[1]
        
        mapdata[jj].l     = minmax(xval)
        mapdata[jj].b     = minmax(yval)
        
     ENDELSE
     ;; Save ASTR structure for each map to be used later
     astrs.add, astr, jj
     
     ;; Keep memory clear
     undefine,xval,yval,hdr,astr,nhd,lhd
     
  ENDFOR
  
  
  ;;===================================================================
  ;; Loop through each survey source, and fill in the survey structure.
  message,'Looping through catalog sources...',/inf
  FOR ii=start, ncat-1 DO BEGIN
     
     ;; Check this object's position against the mapdata
     ;;   structure, with cases for glon near 0 deg.
     hit = WHERE( s[ii].glon GE mapdata.l[0] AND $
                  s[ii].glon LE mapdata.l[1] AND $
                  s[ii].glat GE mapdata.b[0] AND $
                  s[ii].glat LE mapdata.b[1], nhit )
     s[ii].glon -= 360.
     hitm = WHERE( s[ii].glon GE mapdata.l[0] AND $
                   s[ii].glon LE mapdata.l[1] AND $
                   s[ii].glat GE mapdata.b[0] AND $
                   s[ii].glat LE mapdata.b[1], nhitm )
     s[ii].glon += 720.
     hitp = WHERE( s[ii].glon GE mapdata.l[0] AND $
                   s[ii].glon LE mapdata.l[1] AND $
                   s[ii].glat GE mapdata.b[0] AND $
                   s[ii].glat LE mapdata.b[1], nhitp )
     s[ii].glon -= 360.
     
     ;; Check that we found something
     nmatch = nhit + nhitm + nhitp
     IF nmatch EQ 0 THEN BEGIN
        message,'Error: No map match for catalog #'+$
                string(s[ii].cnum,format=fmt),/inf
        CONTINUE
     ENDIF
     
     ;; Re-aggregate the results
     hits = hit
     IF nhitm THEN hits = [hits,hitm]
     IF nhitp THEN hits = [hits,hitp]
     ;; Remove any "-1" entries from hits 
     hits = hits[missing([-1],hits)]
     
     IF nmatch NE n_elements(hits) THEN $
        message,"You've got problems with your number of hits...  STOP!"
     
     ;;===================================================================
     ;; If nmatch = 1, we are good to go, but we need to figure out
     ;;   which image should be used for objects with positions in
     ;;   more than one map.
     
     IF nmatch NE 1 THEN BEGIN
        ;; This process is facilitated if label maps are provided
        IF conf.haslabel THEN BEGIN
           
           bestjj = -1
           ;; Loop through the label maps, run adxy, check for non-zero
           ;;   value at location of peak flux density.
           FOR ll=0,nmatch-1 DO BEGIN
              jj = hits[ll]
              adxy,lhds[jj],s[ii].glon,s[ii].glat,x,y
              x = long(round(x))
              y = long(round(y))
              IF( x LT 0 || x GE mapdata[jj].naxis[0] ) THEN CONTINUE
              IF( y LT 0 || y GE mapdata[jj].naxis[1] ) THEN CONTINUE
              val = (labels[jj])[x,y]
              IF val NE 0 THEN bestjj = jj
           ENDFOR  
           jj = bestjj
        ENDIF ELSE BEGIN
           ;; Else, this is a little messier
           
           ;; Loop through the data maps, run ad2xy, check for finite
           ;;   data value at the location of peak flux density
           bestjj = -1
           bestx = 0
           FOR ll=0,nmatch-1 DO BEGIN
              jj = hits[ll]
              ad2xy,s[ii].glon,s[ii].glat,astrs[jj],x,y
              IF( x LT 0 || x GE mapdata[jj].naxis[0] ) THEN CONTINUE
              
              ;; Check that there are no NaNs within the vicinity of
              ;;   this location in the map  (5x5 box)
              xb = ((long(round(x)) + [-2,2]) > 0) < (mapdata[jj].naxis[0]-1)
              yb = ((long(round(y)) + [-2,2]) > 0) < (mapdata[jj].naxis[1]-1)
              val = (maps[jj])[xb[0]:xb[1],yb[0]:yb[1]]
              IF fix(total(~finite(val))) THEN CONTINUE
              
              ;; Select the map for which this point lies farthest from
              ;;   the edge.  ----- NOT RIGHT METRIC!!!1!
              xedge = long(round(abs(x) < abs(mapdata[jj].naxis[0]-x-1)))
              IF xedge GT bestx THEN BEGIN
                 bestx = xedge
                 bestjj = jj
              ENDIF
           ENDFOR
           jj = bestjj
        ENDELSE
     ENDIF ELSE jj = hits       ; End of the nmatch > 1 checking section
     ;;===================================================================
     
     jj = jj[0]                 ; Make scalar, else all goes to hell.
     
     ;;=======================================================
     ;; Get the survey image mapname, and place in structure
     ;; Get the peak position of the survey source in the image
     ad2xy,s[ii].glon,s[ii].glat,astrs[jj],x,y
     
     x = (long(round(x)))[0]    ; SCALAR!
     y = (long(round(y)))[0]    ; SCALAR!
     
     ;; If X or Y is outside the map (shouldn't happen), then
     ;;   continue to next object...
     IF( x LT 0 || x GE mapdata[jj].naxis[0] ) THEN BEGIN
        message,'Warning: X coordinate out of bounds for '+conf.survey+$
                ' #'+string(survey[ii].cnum,format=fmt)+'.  XPOS = '+$
                string(x,format="(I0)")+' bounds = [0,'+$
                string(mapdata[jj].naxis[0],format="(I0)")+']  '+$
                'Skipping to next object.',/inf
        CONTINUE
     ENDIF
     IF( y LT 0 || y GE mapdata[jj].naxis[1] ) THEN BEGIN
        message,'Warning: Y coordinate out of bounds for '+conf.survey+$
                ' #'+string(survey[ii].cnum,format=fmt)+'.  YPOS = '+$
                string(y,format="(I0)")+' bounds = [0,'+$
                string(mapdata[jj].naxis[1],format="(I0)")+']  '+$
                'Skipping to next object.',/inf
        CONTINUE
     ENDIF
     
     ;; Load up the SURVEY structure with the relevant information
     survey[ii].mapname = $
        strmid(survey_maps[jj],strpos(survey_maps[jj],'/',/REVERSE_SEARCH)+1)
     IF conf.haslabel THEN $
        survey[ii].labelname = $
        strmid(survey_label[jj],strpos(survey_label[jj],'/',/REVERSE_SEARCH)+1)
     IF conf.hassmooth THEN $
        survey[ii].smooname = $
        strmid(survey_smoo[jj],strpos(survey_smoo[jj],'/',/REVERSE_SEARCH)+1)
     survey[ii].xpos = x
     survey[ii].ypos = y
     
     IF conf.haslabel THEN BEGIN
        survey[ii].labval = (labels[jj])[x,y]                ; Set LABVAL
        isource = where(labels[jj] EQ survey[ii].labval,nxy) ; Find pixels
        survey[ii].npix = nxy                                ; Record NPIX
        survey[ii].maxflux = max((maps[jj])[isource])        ; Record MAXFLUX
        
        ;; Compute mean of noise map within label contour
        IF conf.hasnoise THEN $
           survey[ii].noise = mean((noises[jj])[isource])
     ENDIF
     
      
  ENDFOR                        ; End of source loop
  
  ;; Save all this to disk for later joy
  survey_info = conf
  save,survey,survey_info,/verbose,$
       filename='./local/'+conf.survey+'_map_locations.sav'
  
  ;; Clean up the memory
  undefine,survey
  
END
