;+
; NAME:
;       OMNI_SURVEY_POSTAGE
;
; PURPOSE:
;       Creates a postage-stamp image for each survey catalog source,
;       centered on the pixel of peak emission (or whatever GLON/GLAT
;       is reported in the survey catalog).
;
; CATEGORY:
;       distance-omnibus Local File Creation
;
; CALLING SEQUENCE:
;       OMNI_SURVEY_POSTAGE [,CONFFILE=cfile][,/PLOT]
;                           [,START=start][,REAR=rear]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
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
;                    last entry in the catalog.
;
; KEYWORD PARAMETERS:
;       /PLOT   -- Display plots to screen rather than writing .FITS
;                  files.  Only use for diagnostic purposes.
;
; OUTPUTS:
;       Writes FITS file for each catalog source to the directory
;       'postage' directory specified in local_layout.conf.  If
;       keyword /PLOT is set (i.e. plot to screen), FITS files will
;       not be generated. 
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; EXAMPLE:
;       omni_survey_postage,size=8.5,start=8000,/PLOT
;       omni_survey_postage
;
; MODIFICATION HISTORY:
;       Created:  08/27/10, TPEB -- Finally codified and documented
;                                   this procedure.
;       Modified: 04/29/11, TPEB -- Changed keyword NOPLOT -> PLOT, so
;                                   that the default action is to
;                                   create the .FITS files.  Also,
;                                   added HISTORY to FITS header
;                                   including routine name and date of
;                                   creation.  Plus, code cleanup.
;       Modified: 07/18/11, TPEB -- Added FITS keyword ORIGFILE and
;                                   FILENAME to postage-stamp files to
;                                   make finding the originating file
;                                   easier.
;       Modified: 07/19/11, TPEB -- Added check for correct PPBEAM
;                                   FITS keyword in the BGPS header.
;       Modified: 09/12/11, TPEB -- Updated START/REAR/CNUM_LIST to
;                                   conform in usage with other
;                                   routines in the distance-omnibus
;                                   project.
;       Modified: 12/09/11, TPEB -- Modified way postage stamp
;                                   subsection is defined (also
;                                   removes conflict with keyword
;                                   START).
;       Modified: 02/26/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.
;       Modified: 02/27/13, TPEB -- Added COMMENT='#' to the readcol
;                                   calls to allow for comments in the
;                                   filename list files; set POSTAGE_D
;                                   from local_layout.conf config
;                                   file; set SIZE from local_layout,
;                                   also.
;       Modified: 02/28/13, TPEB -- Now gets PIXSCALE from image
;                                   header rather than config file;
;                                   also added COMMON block for
;                                   configuration structures.
;       Modified: 06/03/13, TPEB -- Corrected bug (typo) in name of
;                                   astrometry structure.  Added check
;                                   for sources outside image bounds
;                                   for generalizability.  Made fully
;                                   compliant with NOPARAMS output
;                                   from EXTAST.  Write 0 label mask
;                                   for object positions with no label
;                                   at coordinates.
;       Modified: 08/01/13, TPEB -- Spruced up the "Working..." output
;                                   to be prettier.
;       Modified: 06/30/14, TPEB -- Add check for appropriate IDL
;                                   version.
;
;-

PRO OMNI_SURVEY_POSTAGE, CONFFILE=cfile, START=start, REAR=rear, $
                         CNUM_LIST = cnum_list, PLOT=plot
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  omni_check_version            ; Check for an appropriate IDL version
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Parse keyword parameters
  plot = KEYWORD_SET(plot)
  
  ;; Read in the configuration file
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN
  ENDIF
  
  ;; Restore the map locations structure SURVEY -- if it
  ;;   doesn't yet exist, run OMNI_ASSOC_CATALOG.pro
  mlfn = 'local/'+conf.survey+'_map_locations.sav'
  IF ~FILE_TEST(mlfn,/READ) THEN omni_assoc_catalog,CONFFILE=cfile
  restore,mlfn,/verbose
  
  ;; Read local_layout.conf
  IF ~ exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN
  ENDIF
  POSTAGE_D = local.postage
  size = (local.pssize EQ 0) ? 8.5 : local.pssize
  
  ;; Set up the plot environment, if desired
  IF plot THEN BEGIN
     set_plot,'x'
     window,2
     cgLoadct,3
  ENDIF
  
  ;; Read in the survey catalog & count entries
  s = omni_read_cat(conf.cat,ncat,fmt)
  fmt2 = string(ceil(alog10(ncat+1)),format="('I',I0)")
  
  ;; If CNUM_LIST supplied, then only do the big loop for those objects
  IF n_elements(cnum_list) NE 0 THEN BEGIN
     n_obj = n_elements(cnum_list) 
     ind   = WHERE_ARRAY(cnum_list, s.cnum)
     s     = s[ind]
     start = 0L
     rear  = n_obj-1
     plot  = 1b                 ; Since this is for debugging, auto set /PLOT
  ENDIF ELSE BEGIN
     ;; Else, look for START & REAR keywords and adjust accordingly
     n_obj = ncat
     start = (n_elements(start) NE 0) ? long(start) : 0
     rear  = (n_elements(rear)  NE 0) ? long(rear)  : n_obj-1
  ENDELSE  
  
  ;; Find all the survey data mosaics, label maps, and smoothed images
  readcol,conf.maps,survey_maps,format='a',count=n_map,/SILENT,comment='#'
  mapname = strarr(n_map)
  IF conf.haslabel THEN BEGIN
     readcol,conf.label,survey_label,format='a',count=n_label,$
             /SILENT,comment='#'
     labname = strarr(n_label)
  ENDIF ELSE  n_label = 0
  IF conf.hassmooth THEN BEGIN
     readcol,conf.smooth,survey_smoo,format='a',count=n_smoo,$
             /SILENT,comment='#'
     smoname = strarr(n_smoo)
  ENDIF ELSE n_smoo = 0
  
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
  
  q = '"'    ; Needed for command/Execute pairs
  ;; Read the map images & headers to memory (for faster processing)
  FOR j=0L, n_label-1 DO BEGIN
     
     command = string(j,q,survey_maps[j],q,j,format=$
                      "('image',I0,' = readfits(',A0,A0,A0,',hdr',I0,')')")
     errcode = Execute(command)
     mapname[j] = strmid(survey_maps[j],$
                         strpos(survey_maps[j],'/',/reverse_search)+1)
     
     IF conf.haslabel THEN BEGIN
        command = string(j,q,survey_label[j],q,j,format=$
                         "('label',I0,' = readfits(',A0,A0,A0,',lhdr',I0,')')")
        errcode = Execute(command)
        labname[j] = strmid(survey_label[j],$
                            strpos(survey_label[j],'/',/reverse_search)+1)
     ENDIF
     
     IF conf.hassmooth THEN BEGIN
        command = string(j,q,survey_smoo[j],q,j,format=$
                         "('smooth',I0,' = readfits(',A0,A0,A0,',shdr',I0,')')")
        errcode = Execute(command)
        smoname[j] = strmid(survey_smoo[j],$
                            strpos(survey_smoo[j],'/',/reverse_search)+1)
     ENDIF
     
  ENDFOR
  
  
  ;;Loop through SURVEY sources
  lastj = -1                    ; Used to keep track of previous map image
  FOR i=start, rear DO BEGIN
     
     IF (i+1) MOD 500 EQ 0 THEN $
        message,'Working '+conf.survey+' object #'+$
                string(s[i].cnum,format='('+fmt2+')'),/inf
     
     ;; Get image index # for extraction
     j = WHERE(mapname EQ survey[i].mapname, nj)
     
     ;; Speed up processing by using settings from previous loop, if possible
     IF (j NE lastj) THEN BEGIN
        
        ;; Map Data
        command = string(j,format="('image = image',I0)")
        errcode = Execute(command)
        command = string(j,format="('hdr = hdr',I0)")
        errcode = Execute(command)
        
        IF conf.haslabel THEN BEGIN
           command = string(j,format="('label = label',I0)")
           errcode = Execute(command)
           command = string(j,format="('lhdr = lhdr',I0)")
           errcode = Execute(command)
        ENDIF
        
        IF conf.hassmooth THEN BEGIN
           command = string(j,format="('smooth = smooth',I0)")
           errcode = Execute(command)
           command = string(j,format="('shdr = shdr',I0)")
           errcode = Execute(command)
        ENDIF        
        
        ;; Check the FITS header for the correct value of PPBEAM, and
        ;;    write the corrected value to disk, if necessary. 
        IF conf.ppbeam NE 0 THEN $
           IF omni_check_ppbeam(hdr,conf) THEN $
              writefits,survey_maps[j],image,hdr
        
        IF plot THEN plotmap,image,hdr,tit=survey[i].mapname
        
        ;; Calculate the radius in pixels required for the box size
        ;;   specified by SIZE.
        extast,hdr,astr,ch_par
        CASE ch_par OF          ; See documentation for EXTAST
           1: pixscale = abs(astr.cdelt[0])*3600.
           3: pixscale = abs(astr.cdelt[0])*3600.
           2: pixscale = abs(astr.cd[0,0]) * 3600.
           ELSE: message,'Error: Astrometry information for '+$
                         survey[i].mapname+' is not useable!'
        ENDCASE
        radius = long(round(size * 60. / pixscale / 2.))
        
        ;; Record which of survey_maps this is for the next loop
        lastj = j
     ENDIF
     
     ;; Define subsection parameters for this SURVEY source
     ;; Determine box corners for postage stamp extraction, staying
     ;;   within image bounds
     imgsz = size(image,/DIM)
     box   = [ (survey[i].xpos - radius) > 0, $
               (survey[i].xpos + radius) < (imgsz[0]-1), $
               (survey[i].ypos - radius) > 0, $
               (survey[i].ypos + radius) < (imgsz[1]-1) ]
     
     ;; Check for catalog source lying outside the bounds (for
     ;; generalization of the code) -- skip to next source.
     IF (survey[i].xpos LT 0) || (survey[i].ypos LT 0) || $
        (survey[i].xpos GT (imgsz[0]-1)) || $
        survey[i].ypos GT (imgsz[1]-1) THEN CONTINUE
     
     ;; Use HEXTRACT to extract the subsection and update header WCS
     hextract,image,hdr,ps,pshdr,box[0],box[1],box[2],box[3],/silent
     IF conf.hassmooth THEN $
        hextract,smooth,shdr,pss,psshdr,box[0],box[1],box[2],box[3],/silent
     
     ;; Make the pl image (label postage-stamp) just the source mask
     IF conf.haslabel THEN BEGIN
        hextract,label,lhdr,pl,plhdr,box[0],box[1],box[2],box[3],/silent
        CASE label[survey[i].xpos,survey[i].ypos] GE 1 OF
           1: pl = pl EQ label[survey[i].xpos,survey[i].ypos] ; Yes object
           0: pl = byte(pl * 0b)                              ; No object
        ENDCASE
     ENDIF
     
     ;; Write the postage stamp (image and label) to FITS files  (but
     ;; only if not plotting to screen)
     IF ~plot THEN BEGIN
        
        ;; Add SURVEY CNUM & Object Name and HISTORY to the FITS headers
        ;; including date of modification and the routine name.
        
        hist = ['OMNI_SURVEY_POSTAGE: '+systime(0),$
                'Created '+conf.survey+' postage-stamp image']
        GET_DATE,dte
        
        
        ;; SURVEY science image
        psfn  = conf.survey+'_data'+string(s[i].cnum,format=fmt)+'.fits'
        sxaddpar, pshdr, 'DATE_PS',dte, AFTER='DATE', $
                  ' Creation UTC date of postage-stamp FITS file'
        sxaddpar, pshdr, 'OBJNAME' ,s[i].name, AFTER='DATE_PS', $
                  ' '+conf.survey+' Object Name in Galactic Coordinates'
        sxaddpar, pshdr, 'CNUM', s[i].cnum, AFTER='OBJNAME', $
                  ' '+conf.survey+' catalog number'
        sxaddpar, pshdr, 'ORIGFILE', (mapname[j])[0], AFTER='CNUM', $
                  ' Original '+conf.survey+' map'
        sxaddpar, pshdr, 'FILENAME', psfn, AFTER='ORIGFILE'
        sxaddhist, hist, pshdr
        mwrfits,ps,POSTAGE_D+psfn,pshdr,/create
        
        ;; SURVEY label map
        IF conf.haslabel THEN BEGIN
           plfn  = conf.survey+'_label'+string(s[i].cnum,format=fmt)+'.fits'
           sxaddpar, plhdr, 'DATE_PS',dte, AFTER='DATE', $
                     ' Creation UTC date of postage-stamp FITS file'
           sxaddpar, plhdr, 'OBJNAME' ,s[i].name, AFTER='DATE_PS', $
                     ' '+conf.survey+' Object Name in Galactic Coordinates'
           sxaddpar, plhdr, 'CNUM', s[i].cnum, AFTER='OBJNAME', $
                     ' '+conf.survey+' catalog number'
           sxaddpar, plhdr, 'ORIGFILE', (labname[j])[0], AFTER='CNUM', $
                     ' Original '+conf.survey+' label file'
           sxaddpar, plhdr, 'FILENAME', plfn, AFTER='ORIGFILE'
           sxaddhist, hist, plhdr
           mwrfits,pl,POSTAGE_D+plfn,plhdr,/create ;,/silent
        ENDIF
        
        ;; Smoothed image
        IF conf.hassmooth THEN BEGIN
           pssfn = conf.survey+'_smooth'+string(s[i].cnum,format=fmt)+'.fits'
           sxaddpar, psshdr, 'DATE_PS',dte, AFTER='DATE', $
                     ' Creation UTC date of postage-stamp FITS file'
           sxaddpar, psshdr, 'OBJNAME' ,s[i].name, AFTER='DATE_PS', $
                     ' '+conf.survey+' Object Name in Galactic Coordinates'
           sxaddpar, psshdr, 'CNUM', s[i].cnum, AFTER='OBJNAME', $
                     ' '+conf.survey+' catalog number'
           sxaddpar, psshdr, 'ORIGFILE', (smoname[j])[0], AFTER='CNUM', $
                     ' Original '+conf.survey+' smoothed image file'
           sxaddpar, psshdr, 'FILENAME', pssfn, AFTER='ORIGFILE'
           sxaddhist, hist, psshdr
           mwrfits,pss,POSTAGE_D+pssfn,psshdr,/create
        ENDIF
        
     ENDIF ELSE BEGIN           ; End of NOPLOT, start of PLOT
        
        titlestr = string(s[i].cnum,s[i].glon,s[i].glat,format=$
                          "('SURVEY #',I4,'  l = ',F5.1,'  b = ',F5.2)")
        
        ;; Plot to screen.  Includes wait time for visual inspection
        plotmap,ps,pshdr,range=set_plot_range(ps*(pl*0.65 + 0.35)),$
                tit=titlestr,XC=xc,YC=yc
        
        ;; Plot contours
        cgContour,pl,xc,yc,levels=[0.5],thick=3,color='yellow',/over
        cgPlots,s[i].glon,s[i].glat,psym=2,thick=3,symsize=2,color='Lime Green'
        
        wait,0.5
     ENDELSE
     
  ENDFOR                        ; End of loop through survey sources
  
  RETURN  
END
