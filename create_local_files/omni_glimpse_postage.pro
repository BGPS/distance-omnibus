;+
; NAME:
;       OMNI_GLIMPSE_POSTAGE
;
; PURPOSE:
;       Creates a postage-stamp image from the GLIMPSE data set (IRAC
;       Band 4) for each survey catalog source within the GLIMPSE
;       bounds (-65 < l < 65 ; -1 < b < 1).  Takes size information
;       from the local_layout.conf configuration file.
;
; CATEGORY:
;       distance-omnibus Local File Creation
;
; CALLING SEQUENCE:
;       OMNI_GLIMPSE_POSTAGE [,CONFFILE=cfile][,/PLOT]
;                            [,START=start][,REAR=rear]
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
;                    last entry in the catalog]
;
; KEYWORD PARAMETERS:
;       /PLOT   -- Display plots to screen rather than writing .FITS
;                  files.  Only use for diagnostic purposes.
;
; OUTPUTS:
;       Writes FITS file of the GLIMPSE postage stamp around each
;       catalog source to the 'postage' directory specified in
;       local_layout.conf. If keyword /PLOT is set (i.e. plot to
;       screen), then FITS files will not be generated. 
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
;       Created:  08/27/10, TPEB -- Initial version, closely following
;                                   bgps_postage.pro
;       Modified: 10/21/10, TPEB -- Documentation of features, and
;                                   code cleanup
;       Modified: 02/15/11, TPEB -- Make GLIMPSE postage stamp images
;                                   from the V3.5 GLIMPSE data
;                                   (background and boundary matched)
;       Modified: 04/29/11, TPEB -- Changed keyword NOPLOT -> PLOT, so
;                                   that the default action is to
;                                   create the .FITS files.  Also,
;                                   added HISTORY to FITS header
;                                   including routine name and date of
;                                   creation.  Plus, code and
;                                   documentation cleanup.
;       Modified: 07/18/11, TPEB -- Added FITS keyword ORIGFILE and
;                                   FILENAME to postage-stamp files to
;                                   make finding the originating file
;                                   easier.
;       Modified: 09/12/11, TPEB -- Updated START/REAR/CNUM_LIST to
;                                   conform in usage with other
;                                   routines in the distance-omnibus
;                                   project.
;       Modified: 12/09/11, TPEB -- Modified way postage stamp
;                                   subsection is defined (also
;                                   removes conflict with keyword
;                                   START).
;       Modified: 02/27/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.
;       Modified: 02/28/13, TPEB -- Moved pixscale calculation to be
;                                   done once per loaded image; also
;                                   added COMMON block for
;                                   configuration structures.
;       Modified: 06/03/13, TPEB -- Made fully compliant with NOPARAMS
;                                   output from EXTAST.
;
;-

PRO OMNI_GLIMPSE_POSTAGE, CONFFILE=cfile, PLOT=plot, START=start, $
                          REAR=rear, CNUM_LIST = cnum_list
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
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
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN
  ENDIF
  POSTAGE_D = local.postage
  size = (local.pssize EQ 0) ? 8.5 : local.pssize
  
  ;; Get list of IRAC Band 4 images
  readcol,local.glimpse,glimpse,format='a',count=n_gl,/SILENT,comment='#'
  gind = where(strmatch(glimpse,'*I4*',/fold),ng4)
  IF ng4 EQ 0 THEN BEGIN
     message,'Error: File '+local.glimpse+$
             ' does not contain IRAC Band 4 GLIMPSE images.  Exiting.',/cont
     RETURN
  ENDIF
  glimpse = glimpse[gind]
  
  ;; Set up the plot environment, if desired
  IF plot THEN BEGIN
     set_plot,'x'
     window,2
     cgLoadct,3
  ENDIF
  
  ;; Read in the survey catalog & count entries
  s = omni_read_cat(conf.cat,ncat,fmt)
  
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
  
  
  ;; Loop though SURVEY sources
  lastfn = ''                   ; Used to keep track of previous GLIMPSE image
  FOR i=start, rear DO BEGIN
     
     ;; Check to see if the SURVEY source lies within GLIMPSE coverage
     IF (s[i].glon LE 65.25 && s[i].glon GE -65. ) || $
        s[i].glon GE 295. THEN BEGIN
        
        ;; Determine GLIMPSE image needed for extraction -- mosaics
        ;;    are arranged every 3 degrees in GLON
        glon  = round(s[i].glon/3.)*3.
        sglon = (glon EQ 360) ? '00000' : string(glon,format="(I03)")+'00'
        ifn = where(strmatch(glimpse,'*'+sglon+'*'),nfn)
        IF nfn EQ 0 THEN BEGIN
           message,'Error: File '+local.glimpse+$
                   ' does not contain image for GLON '+sglon+'.  Exiting.',/cont
           RETURN
        ENDIF
        
        fn = glimpse[ifn]
        IF fn NE lastfn THEN BEGIN
           print,fn
           irac = readfits(fn,hdr)
           extast,hdr,glastr,ch_par
           CASE ch_par OF       ; See documentation for EXTAST
              1: pixscale = abs(glastr.cdelt[0])*3600.
              3: pixscale = abs(glastr.cdelt[0])*3600.
              2: pixscale = abs(glastr.cd[0,0]) * 3600.
              ELSE: message,'Error: Astrometry information for '+$
                            fn+' is not useable!'
           ENDCASE
           glsize = size(irac,/DIM)
           ;; Calculate the radius in pixels required for the box size
           ;;   specified by SIZE.  conf.pixscale MUST be set
           radius = long(round(size * 60. / pixscale / 2.))
           
           lastfn = fn
        ENDIF
        
        ;; Get coordinates in the GLIMPSE image of SURVEY PEAK emission
        ad2xy, s[i].glon, s[i].glat, glastr, x, y
        cent = long(round([x,y]))
        
        ;; Define subsection parameters for this SURVEY source
        ;; Determine box corners for postage stamp extraction, staying
        ;;   within image bounds
        box = [ (cent[0]-radius) > 0, $
                (cent[0]+radius) < (glsize[0]-1), $
                (cent[1]-radius) > 0, $
                (cent[1]+radius) < (glsize[1]-1) ]
        
        ;; Check for being outside image
        IF (box[1] LE box[0] OR box[3] LE box[2]) THEN BEGIN
           print,'Blank!',s[i].glon 
           CONTINUE             ; Create no FITS image for this SURVEY source
        ENDIF
        
        ;; Use HEXTRACT to extract the subsection, and update header WCS
        hextract,irac,hdr,ps,pshdr,box[0],box[1],box[2],box[3],/silent
        
        ;; Write the GLIMPSE postage stamp to FITS files  (but only if
        ;; not plotting to screen).
        IF ~plot THEN BEGIN
           
           ;; Add HISTORY to the FITS headers including date of modification
           ;; and the routine name.
           hist = ['OMNI_GLIMPSE_POSTAGE: '+systime(0),$
                   'Created GLIMPSE postage-stamp image']
           GET_DATE,dte
           
           psfn = conf.survey+'_irac'+string(s[i].cnum,format=fmt)+'.fits'
           
           ;; GLIMPSE image
           sxaddpar, pshdr, 'DATE_PS',dte, AFTER='DATE', $
                     ' Creation UTC date of postage-stamp FITS file'
           sxaddpar, pshdr, 'OBJNAME' ,s[i].name, AFTER='DATE_PS', $
                     ' '+conf.survey+' Object Name in Galactic Coordinates'
           sxaddpar, pshdr, 'CNUM', s[i].cnum, $
                     ' '+conf.survey+' catalog number', AFTER='OBJNAME'
           glfn = strmid(lastfn,strpos(lastfn,'/',/REVERSE_SEARCH)+1)
           sxaddpar, pshdr, 'ORIGFILE', glfn[0], AFTER='CNUM', $
                     ' Original GLIMPSE file'
           sxaddpar, pshdr, 'FILENAME', psfn, AFTER='ORIGFILE'
           sxaddhist, hist, pshdr
           
           ;; Write the FITS file
           mwrfits,ps,POSTAGE_D+psfn,pshdr,/create
           
        ENDIF ELSE BEGIN        
           ;; Plot to screen
           plotimage,ps,/preserve_aspect,range=set_plot_range(ps),xst=4,yst=4
           plot_wcs_axes,pshdr,1
           wait,0.5
        ENDELSE
        
     ENDIF ELSE BEGIN           ; Create no FITS image for this SURVEY source
        print,'Blank!',s[i].glon
        
     ENDELSE
  ENDFOR
  
  RETURN
END
