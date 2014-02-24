;+
; NAME:
;       IGPS_CONTINUUM_POSTAGE
;
; PURPOSE:
;       Creates a postage-stamp image from the IGPS 21-cm continuum
;       data set for each BGPS source.  Takes WCS and size information
;       from the BGPS postage stamp images.
;
; CATEGORY:
;       distance-omnibus
;
; CALLING SEQUENCE:
;       IGPS_CONTINUUM_POSTAGE [,/PLOT][,START=start][,REAR=rear]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
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
;       /PLOT   -- Display plots to screen rather than writing .FITS files.
;
; OUTPUTS:
;       Writes FITS file of the IGPS continuum image postage stamp
;       around each  of each BGPS source in the directory
;       ./local/postage  If keyword /PLOT is set (i.e. plot to
;       screen), then FITS files will not be generated. 
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;       Created:  09/02/10, TPEB -- Initial version, closely following
;                                   bgps_postage.pro
;       Modified: 04/29/11, TPEB -- Moved continuum filename 'get' to
;                                   GET_HI_CUBE.pro.  Changed keyword
;                                   NOPLOT -> PLOT, so that the
;                                   default action is to create the
;                                   .FITS files.  Also, added HISTORY
;                                   to FITS header including routine
;                                   name and date of creation.  Plus,
;                                   code and documentation cleanup. 
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
;       Modified: 06/03/13, TPEB -- Made fully compliant with NOPARAMS
;                                   output from EXTAST.
;
;-


PRO IGPS_CONTINUUM_POSTAGE, PLOT=plot, START=start, REAR=rear, $
                            CNUM_LIST=cnum_list
  
  ;; Parse keyword parameters
  IF ~ KEYWORD_SET( radius ) THEN radius = 35
  do_plot = KEYWORD_SET( plot )
  IF ~ KEYWORD_SET( bgps_ml ) THEN $
     restore,'local/BGPS_v102_map_locations.sav',/verbose ELSE $
        restore,bgps_ml,/ver
  
  ;; Set up the plot environment, if desired
  IF do_plot THEN BEGIN
     set_plot,'x'
     window,2
     cgLoadct,3
  ENDIF
  
  ;; Read in the BGPS Distance Database CSV file
  s = read_bgps_csv('bgps_distance_database.csv',csv,/VERBOSE)
  
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
  
  ;; Set up directory locations, and get BGPS Map and Label files
  LABEL_DIR = 'local/bgps/label/'
  MAP_DIR   = 'local/bgps/maps/'
  POSTAGE_D = 'local/postage/'
  hi_dirs = ['./local/sgps/','./local/vgps/','./local/cgps/']
  bgps_list = FILE_SEARCH(LABEL_DIR+'*.fits.gz', COUNT=n_list)
  bgps_maps = FILE_SEARCH(MAP_DIR  +'*.fits', COUNT=n_map)
  q = '"'
  
  ;; We'll need to load IGPS images on-the-fly a la HISA
  lastfn = ''
  
  ;; Loop though BGPS sources
  FOR i=start, rear DO BEGIN
     
     ;; Check to see if the BGPS source lies within IGPS 21-cm coverage
     ;; Check to see if this BGPS source lies within the IGPS coverage

     
     IF (s[i].glon_peak GE 4.5 AND s[i].glon_peak LE 175) OR $
        (s[i].glon_peak GE 349  AND s[i].glon_peak LE 358) THEN BEGIN
        
        ;; Determine IGPS image needed for extraction
        junk = GET_HI_CUBE(s[i].glon_peak, s[i].glat_peak, hi_dirs, fn)
        
        IF fn NE lastfn THEN BEGIN
           print,fn
           igps = readfits(fn,hdr)
           extast,hdr,glastr,ch_par
           CASE ch_par OF       ; See documentation for EXTAST
              1: pixscale = abs(glastr.cdelt[0])*3600.
              3: pixscale = abs(glastr.cdelt[0])*3600.
              2: pixscale = abs(glastr.cd[0,0]) * 3600.
              ELSE: message,'Error: Astrometry information for '+$
                            fn+' is not useable!'
           ENDCASE
           glsize = size(igps,/DIM)
        ENDIF
        lastfn = fn
        
        ;; Get coordinates in the IGPS image of BGPS PEAK emission
        ad2xy, s[i].glon_peak, s[i].glat_peak, glastr, x, y
        cent = round([x,y])
        ;; Read in BGPS label file
        label = readfits(POSTAGE_D+'label'+string(s[i].cnum,format="(I04)")+$
                         '.fits',lhdr,/silent)

        ;; Determine box corners for postage stamp extraction
        sz = size(label,/DIM)
        radius = round((sz - 1) * (7.2 / pixscale / 3600.))/2.
        box = long( [ (cent[0]-radius[0])>0, $
                      (cent[0]+radius[0])<(glsize[0]-1), $
                      (cent[1]-radius[1])>0, $
                      (cent[1]+radius[1])<(glsize[1]-1) ] )
        ;; Check for being just outside image
        IF (box[1] LE box[0] OR box[3] LE box[2]) THEN BEGIN
           print,'Blank!',s[i].glon_peak
           CONTINUE
        ENDIF
        
        ;; Use HEXTRACT to extract the subsection, and update header WCS
        hextract,igps,hdr,ps,pshdr,box[0],box[1],box[2],box[3],/silent

        ;; Write the GLIMPSE postage stamp to FITS files  (but only if
        ;; not plotting to screen).
        IF ~ do_plot THEN BEGIN
                      
           psfn = 'igps'+string(s[i].cnum,format="(I04)")+'.fits'

           ;; Add HISTORY to the FITS headers including date of modification
           ;; and the routine name.
           hist = ['IGPS_CONTINUUM_POSTAGE: '+systime(0),$
                   'Created IGPS (21-cm) Continuum postage-stamp image']
           GET_DATE,dte
           
           sxaddpar, pshdr, 'DATE_PS',dte, $
                     ' Creation UTC date of postage-stamp FITS file', $
                     AFTER='DATE'
           sxaddpar, pshdr, 'OBJNAME' ,s[i].name, $
                     ' BGPS Object Name in Galactic Coordinates', $
                     AFTER='DATE_PS'
           sxaddpar, pshdr, 'CNUM', s[i].cnum, $
                     ' Bolocat (v1.0.1) catalog number', AFTER='OBJNAME'
           igfn = strmid(lastfn,strpos(lastfn,'/',/REVERSE_SEARCH)+1)
           sxaddpar, pshdr, 'ORIGFILE', igfn[0], $
                     ' Original IGPS Continuum file', AFTER='CNUM'
           sxaddpar, pshdr, 'FILENAME', psfn, AFTER='ORIGFILE'
           sxaddhist, hist, pshdr

           
           ;; Write to FITS file
           mwrfits,ps,POSTAGE_D+psfn,pshdr,/create
           
           
        ENDIF ELSE BEGIN
           ;; Plot to screen
           plotimage,ps,/preserve_aspect,range=set_plot_range(ps),xst=4,yst=4
           plot_wcs_axes,pshdr,1
           wait,0.5
        ENDELSE
        
        
     ENDIF ELSE BEGIN ;; Create a blank FITS image for this BGPS source
        print,'Blank!',s[i].glon_peak
        
     ENDELSE
  ENDFOR
  
END
