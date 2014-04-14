;+
; NAME:
;       GRS_POSTAGE
;
; PURPOSE:
;       Creates a postage-stamp image from the GRS data sets for each
;       BGPS source, integrating over the velocity of the dense gas
;       tracer spectrum in the velocity structure (IDL save file).
;       Takes WCS and size information from the BGPS postage stamp images.
;
; CATEGORY:
;       distance-omnibus
;
; CALLING SEQUENCE:
;       GRS_POSTAGE
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
;       Writes FITS file of the velocity-integrated GRS postage stamp
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
;
;       Created:  10/22/10, TPEB -- Initial version, closely following
;                                   hi_postage.pro
;       Modified: 05/01/11, TPEB -- Changed keyword NOPLOT -> PLOT, so
;                                   that the default action is to
;                                   create the .FITS files.  Also,
;                                   added HISTORY to FITS header
;                                   including routine name and date of
;                                   creation.  Plus, code and
;                                   documentation cleanup.
;       Modified: 06/15/11, TPEB -- Replaced DELVARX with UNDEFINE as the
;                                   means for clearing the data cubes
;                                   from memory before loading the next
;                                   one.  Fixed memory leak & improved
;                                   reliability.
;       Modified: 07/18/11, TPEB -- Added FITS keyword ORIGFILE and
;                                   FILENAME to postage-stamp files to
;                                   make finding the originating file
;                                   easier.  Also, brought 'ON' &
;                                   'OFF' maps in line with convention
;                                   from hi_postage.pro.
;       Modified: 09/12/11, TPEB -- Updated START/REAR/CNUM_LIST to
;                                   conform in usage with other
;                                   routines in the distance-omnibus
;                                   project.
;
;-

FUNCTION GET_GRS_CUBE, glon, grs_dir

  ;; Cut-and-paste from grsmatch.pro
  wtorder = sort(glon)
  glon = glon[wtorder]
  lout = strcompress(string(round(glon)), /rem)
  filename = 'grs-'+lout+'-cube.fits'
  fn = grs_dir+filename
  
  RETURN,fn
END

PRO GRS_POSTAGE, PLOT=plot, START=start, REAR=rear, CNUM_LIST=cnum_list
  
  ;; Parse keyword parameters
  do_plot = KEYWORD_SET( plot )
  IF n_elements( bgps_ml ) EQ 0 THEN $
     restore,'local/bgps_v102_map_locations.sav',/verbose ELSE $
        restore,bgps_ml,/ver
  
  
  ;; Read in the BGPS Distance Database CSV file & Velocity Structure
  s = read_bgps_csv('bgps_distance_database.csv',csv,/VERBOSE)
  restore,'local/bgps_velocity_struct.sav',/ver
  
  ;; If CNUM_LIST supplied, then only do the big loop for those objects
  IF n_elements( cnum_list ) NE 0 THEN BEGIN
     n_bgps   = n_elements( cnum_list ) 
     ind      = WHERE_ARRAY(cnum_list, s.cnum)
     s        = s[ind]
     velocity = velocity[ind]
     start    = 0L
     rear     = n_bgps-1
     plot     = 1b         ;; Since this is for debugging, auto set /PLOT
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
  GRS_DIR = './local/grs/'
  bgps_list = FILE_SEARCH(LABEL_DIR+'*.fits.gz', COUNT=n_list)
  bgps_maps = FILE_SEARCH(MAP_DIR  +'*.fits', COUNT=n_map)
  q = '"'
  
  ;; We'll need to load GRS Data Cubes on-the-fly a la GRSMATCH
  lastfn = ''
  
  ;; Loop though BGPS sources
  FOR i=start, rear DO BEGIN
     cnum = string(s[i].cnum,format="(I04)")
     
     message,'Now creating GRS postage stamp for BGPS #'+cnum+'...',/inf
     
     ;; Check to see if this BGPS source lies within the GRS coverage
     good = 0b
     IF (s[i].glon_peak GE 14 AND s[i].glon_peak LE 60) THEN good = 1b
     
     ;; Check to see if this source has a non-zero densegas spectrum
     v = velocity[i]
     IF TOTAL(v.densegas) EQ 0 THEN good = 0b
     
     IF ~ good THEN BEGIN 
        message,/INF,$
               'Cannot make Postage Stamp (outside coverage and/or no spectrum)'
        CONTINUE
     ENDIF
     
     ;; Determine GRS data cube needed for extraction
     fn = get_grs_cube(s[i].glon_peak, grs_dir)
     IF ~ file_test(fn,/READ) THEN CONTINUE
     IF fn NE lastfn THEN BEGIN
        UNDEFINE,cube
        print,fn
        cube = readfits(fn,hdr)
        
        ;; Erik's custom header-reading software
        rdhd, hdr, s = h
        
        extast,hdr,grsastr
        grssize = (size(cube,/DIM))[0:1]
     ENDIF
     lastfn = fn
     
     ;; Get coordinates in the GRS cube of BGPS PEAK emission
     ad2xy, s[i].glon_peak, s[i].glat_peak, grsastr, x, y
     cent = round([x,y])
     
     ;; Read in BGPS label file
     label = readfits(POSTAGE_D+'label'+cnum+'.fits',lhdr,/silent)
     sz = size(label,/DIM)
     
     ;; Determine GRS subsection for extraction...
     subsize = long(round((sz - 1) * (7.2 / grsastr.cdelt[1] / 3600.) + 1.))
     start = cent - round((subsize - 1.) / 2.)
     
     ;; As part of tgrss... need to associate WCS with new subsection...
     ;; Check starting and stopping pixels to ensure in range...
     ind = WHERE(start LT 0, nind)
     IF nind NE 0 THEN start[ind] = 0
     ind = WHERE(start+subsize GT grssize, nind)
     IF nind NE 0 THEN BEGIN
        excess = ((start + subsize) - grssize)>0
        subsize -= excess
     ENDIF
     
     ;; Need to be clever about extracting subsections of tgrss cube --
     ;; also have to integrate...
     
     junk = hdr
     sxdelpar, junk, 'NAXIS3'
     sxdelpar, junk, 'NAXIS4'
     sxaddpar, junk, 'NAXIS', 2
     
     ;; Use HEXTRACT to extract the subsection header, and update hdr WCS
     hextract,cube[*,*,0],junk,ps,pshdr,start[0],start[0]+subsize[0]-1,$
              start[1],start[1]+subsize[1]-1 ,/silent
     
     ;; Figure out wgrsch velocities need to be in the "ON" map
     result = MPFITPEAK(v.v_std,v.densegas, A, NTERMS=3, /POSIT)
     
     ;; First, check that the densegas velocity is within GRS velocity
     ;; coverage
     IF (A[1] LT min(h.v) OR A[1] GT max(h.v)) THEN CONTINUE

     delta_V = A[2] * sqrt(8. * ALOG(2.))
     
     z_ind = WHERE(h.v GE A[1] - delta_V AND h.v LE A[1] + delta_V, nzind)
     
     onimg = fltarr(subsize)
     FOR ii=0L, nzind-1 DO $
        onimg += extract_subsection(cube[*,*,z_ind[ii]], start, subsize)
     onimg /= float(nzind)
     
     ;; Figure out wgrsch velocities need to be in the "OFF" maps
     lo_cent = A[1] - delta_V - 2.5
     up_cent = A[1] + delta_V + 2.5
     z_off_l = WHERE(h.v GE (lo_cent - 2.5) AND h.v LE (lo_cent + 2.5), nzlo)
     z_off_u = WHERE(h.v GE (up_cent - 2.5) AND h.v LE (up_cent + 2.5), nzup)
     
     offimg = fltarr(subsize)
     FOR ii=0L, nzlo-1 DO $
        offimg += extract_subsection(cube[*,*,z_off_l[ii]], start, subsize) $
        / float(nzlo+nzup)
     FOR ii=0L, nzup-1 DO $
        offimg += extract_subsection(cube[*,*,z_off_u[ii]], start, subsize) $
        / float(nzlo+nzup)
     
     grsimg = onimg; - offimg
     
     ;; Check for NaN (off image)
     nanind = WHERE( ~FINITE(grsimg),nnan)
     IF nnan NE 0 THEN grsimg[nanind] = 0.
     
     ;; If grsimg is blank, break
     IF ~ FINITE(TOTAL(grsimg)) THEN BEGIN
        message,'Returning w/o writing postage stamp image...',/inf
        CONTINUE
     ENDIF
     
     ;; Either display image on screen or write it to disk
     IF do_plot THEN BEGIN
        bgps_smoo = readfits(POSTAGE_D+'smooth'+cnum+'.fits',shdr,/silent)
        plot_wcs_axes,shdr,10,lonarr=blarr,latarr=bbarr
        
        pr = set_plot_range(grsimg)
        plotimage,grsimg,range=[0,pr[1]],/preserve_aspect,$
                  title=cnum,/interp,xst=4,yst=4
        plot_wcs_axes,pshdr
        fsc_colorbar,/vert,/right,range=[0,pr[1]],format="(F0.2)",$
                     title='T!dA!n (K)'
        
        contour,bgps_smoo,blarr,bbarr,levels=[.025,.05,0.1,0.25,0.5],$
                c_colors=[1,2,3,4,5]*50,/overplot,thick=1
        
        wait,1
     ENDIF ELSE BEGIN
        
        psfn = 'grs'+cnum+'.fits'
        
        ;; Add HISTORY to the FITS headers including date of modification
        ;; and the routine name.
        hist = ['GRS_POSTAGE: '+systime(0),$
                'Created GRS postage-stamp image']
        GET_DATE,dte
        
        sxaddpar, pshdr, 'DATE_PS',dte, $
                  ' Creation UTC date of postage-stamp FITS file', $
                  AFTER='DATE'
        sxaddpar, pshdr, 'OBJNAME' ,s[i].name, $
                  ' BGPS Object Name in Galactic Coordinates', $
                  AFTER='DATE_PS'
        sxaddpar, pshdr, 'CNUM', s[i].cnum, $
                  ' Bolocat (v1.0.1) catalog number', AFTER='OBJNAME'
        sxaddpar, pshdr, 'VLSR', A[1], FORMAT="(F0.2)",$
                  'LSR Velocity determined from dense gas tracers [km/s]', $
                  AFTER='CNUM'
        sxaddpar, pshdr, 'DELTA_V', delta_V, FORMAT="(F0.2)",$
                  'FWHM of dense gas line profile [km/s]', AFTER='VLSR'
        grfn = strmid(lastfn,strpos(lastfn,'/',/REVERSE_SEARCH)+1)
        sxaddpar, pshdr, 'ORIGFILE', grfn[0], $
                  ' Original GRS data cube file', AFTER='CNUM'
        sxaddpar, pshdr, 'FILENAME', psfn, AFTER='ORIGFILE'
        sxaddhist, hist, pshdr
        
        mwrfits,grsimg,POSTAGE_D+psfn,pshdr,/create
     ENDELSE
     
  ENDFOR
  
END
