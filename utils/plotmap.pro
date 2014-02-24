;+
; NAME:
;       PLOTMAP
;
; PURPOSE:
;       Shortcut to plot a FITS image with properly labeled axes from
;       the FITS header.
;
; CATEGORY:
;       FITS image plotting utility
;
; CALLING SEQUENCE:
;       PLOTMAP, image, hdr [,RANGE=range][,AXSEL=axsel][,LONARR=lonarr]
;                [,LATARR=latarr][,/LOGSCALE][,/NOTSQUARE]
;
; INPUTS:
;       IMAGE -- 2-D image to be plotted
;       HDR   -- FITS header associated with the image (for axis labeling)
;
; OPTIONAL INPUTS:
;       RANGE  -- The map values to be used as the upper and lower
;                 limits for the color conversion.  [Default:
;                 set_plot_range(image)] 
;       AXSEL  -- Selection value to be passed to
;                 PLOT_WCS_AXES. [Default = 1]
;       CT     -- Color table # to load [Default: Do not load a color
;                 table]
;       XRANGE -- Specify the xrange of the image to plot (used for
;                 speeding up the process).  [Default: full image]
;                 MUST BE IN "LONGITUDE ORDER", decreasing to right.
;       YRANGE -- Specify the yrange of the image to plot (used for
;                 speeding up the process).  [Default: full image]
;       IDL Graphics Keywords may be passed (accepted via the _EXTRA command).
;
; KEYWORD PARAMETERS:
;       LOGSCALE  -- Plot the image with logrithmic scaling rather than
;                    linear scaling.
;       NOTSQUARE -- Disable the PRESERVE_ASPECT keyword in PLOTIMAGE
;                    (useful for (l,v) plots).
;       GQ4       -- Pass-through keyword for plot_wcs_axes that uses
;                    conventional (positive) values for GLON in the
;                    4th Galactic Quadrant.
;       CUBE      -- For plotting (l,v) diagram with header from full
;                    data cube, properly sets header parameters for
;                    GLON and VLSR.
;       METERS    -- Used for (l,v) diagrams, where the velocity in
;                    the supplied header is in m/s -- converts to km/s
;                    for plotting.
;       NOPLOT    -- Negate the whole purpose of a plotting routine!
;
; OUTPUTS:
;       Plots the image to the current graphics device with
;       properly-labeled axes.
;
; OPTIONAL OUTPUTS:
;       LONARR -- Array of coordinate values for the edges of each
;                 pixel along the x-axis of the image.
;       LATARR -- Array of coordinate values for the edges of each
;                 pixel along the y-axis of the image.
;       XC     -- Array of coordinate values for each pixel along
;                 the x-axis to be used with cgContour.
;       YC     -- Array of coordinate values for each pixel along
;                 the x-axis to be used with cgContour.
;       RANGE  -- The map values to used as the upper and lower
;                 limits for the color conversion.
;       OUTIMG -- The image actually plotted -- trimmed or logged as
;                 desired.
;       OUTHDR -- The header associated with OUTIMG.
;
; PROCEDURES:
;       Uses PLOTIMAGE and PLOT_WCS_AXES
;
; MODIFICATION HISTORY:
;
;       Created:  06/23/11, TPEB -- Initial version, finally got sick
;                                   of typing all these things in each
;                                   time.
;       Modified: 07/14/11, TPEB -- Added pass-through LONARR & LATARR
;                                   keywords to get those arrays out
;                                   of plot_wcs_axes if desired.
;       Modified: 09/14/11, TPEB -- Added LOGSCALE keyword for
;                                   logrithmic image scaling.
;       Modified: 10/05/11, TPEB -- Added NOTSQUARE keyword to disable
;                                   the /PRESERVE_ASPECT call to
;                                   PLOTIMAGE for things like (l,v)
;                                   plots.
;       Modified: 10/26/11, TPEB -- Added CT keyword to allow user to
;                                   specify a color table to load.
;       Modified: 11/04/11, TPEB -- Added XRANGE and YRANGE keywords
;                                   to specficy ranges to be plotted,
;                                   if desired.
;       Modified: 11/10/11, TPEB -- Fixed bug in XRANGE/YRANGE code
;                                   related to pixel indexing.  Also
;                                   moved set_plot_range() command to
;                                   after hextract() so that the range
;                                   mapped to colortable is for
;                                   plotted (sub)image only.  Finally,
;                                   added error checking for XRANGE /
;                                   YRANGE specified outside image
;                                   bounds.
;       Modified: 04/09/12, TPEB -- Moved CT call to just before
;                                   plotimage, and cleaned up the
;                                   documentation.  Also added
;                                   /REVERSE keyword to reverse the
;                                   colortable, if desired.
;       Modified: 08/20/12, TPEB -- Explicitly include MIN_DPI in call
;                                   list, so that when _EXTRA is sent
;                                   to AXIS, we don't get a barf.
;       Modified: 10/24/12, TPEB -- Explicitly added XC & YC as
;                                   pass-throughs to
;                                   plot_wcs_axes.pro.  With move to
;                                   cgAxis in plot_wcs_axes.pro, the
;                                   _EXTRA was passing XC as
;                                   XCHAR... to PLOT, which it
;                                   doesn't like.
;       Modified: 06/18/13, TPEB -- Added optional outputs OUTIMG and
;                                   OUTHDR, as well as the
;                                   accompanying /NOPLOT keyword if
;                                   image trimming is all the user has
;                                   in mind.  Added COMPILE_OPT
;                                   command.
;       Modified: 08/13/13, TPEB -- Convert input image to type float
;                                   before doing anything else.  This
;                                   fixes issues with byte-type images.
;
;-

PRO PLOTMAP, image, hdr, RANGE=range, AXSEL=axsel, LONARR=lonarr, $
             LATARR=latarr, LOGSCALE=logscale, NOTSQUARE=notsquare, $
             GQ4=gq4, CT=ct, XRANGE=xrange, YRANGE=yrange, CUBE=cube, $
             METERS=meters, REVERSE=reverse, MIN_DPI=min_dpi, $
             XC=xc, YC=yc, OUTIMG=pimg, OUTHDR=nhdr, NOPLOT=noplot, _EXTRA=extra
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Parse keywords
  preserve = ~KEYWORD_SET(notsquare)
  plot     = ~KEYWORD_SET(noplot)
  pimg     = KEYWORD_SET(logscale) ? alog(float(image)) : float(image)
  
  jhdr = hdr
  IF KEYWORD_SET( cube ) THEN BEGIN
     IF KEYWORD_SET( meters ) THEN conv = 1.d3 ELSE conv = 1.d
     sxaddpar,jhdr,'NAXIS2',sxpar(hdr,'NAXIS3')
     sxaddpar,jhdr,'CTYPE2',sxpar(hdr,'CTYPE3')
     sxaddpar,jhdr,'CRPIX2',sxpar(hdr,'CRPIX3') 
     sxaddpar,jhdr,'CDELT2',sxpar(hdr,'CDELT3') / conv
     sxaddpar,jhdr,'CRVAL2',sxpar(hdr,'CRVAL3') / conv
  ENDIF
  
  ;; Make replicate header to use with hextract
  nhdr = jhdr
  IF (n_elements(xrange) NE 0) OR (n_elements(yrange) NE 0) THEN BEGIN 
     plot_wcs_axes,hdr,10,LONARR=lonarr,LATARR=latarr,GQ4=gq4,_EXTRA=extra
     
     IF n_elements(xrange) NE 0 THEN BEGIN
        xind = WHERE( lonarr LE xrange[0] AND lonarr GE xrange[1], nx )
        IF nx EQ 0 THEN BEGIN
           bad_ax = 'x range'
           GOTO,error_msg
        ENDIF
        lonarr = lonarr[xind]
     ENDIF ELSE xind = lindgen(n_elements(lonarr))
     
     IF n_elements(yrange) NE 0 THEN BEGIN
        yind = WHERE( latarr GE yrange[0] AND latarr LE yrange[1], ny )
        IF ny EQ 0 THEN BEGIN
           bad_ax = 'y range'
           GOTO,error_msg
        ENDIF
        lonarr = lonarr[xind]
     ENDIF ELSE yind = lindgen(n_elements(latarr))
     
     x0 = (min(xind)-1) > 0
     x1 = (max(xind)+1) < ((size(pimg,/DIM))[0]-1)
     y0 = (min(yind)-1) > 0
     y1 = (max(yind)+1) < ((size(pimg,/DIM))[1]-1)
     hextract, pimg, nhdr, x0, x1, y0, y1, /silent 
  ENDIF
  
  ;; Set the plot range for the image to be plotted
  IF n_elements(range) NE 2 THEN range = set_plot_range(pimg,_EXTRA=extra)
  
  IF n_elements(ct) NE 0 THEN cgLoadct,ct,/silent,REVERSE=reverse
  
  IF plot THEN $
     plotimage,pimg,range=range,preserve_aspect=preserve,xst=4,yst=4,$
               MIN_DPI=min_dpi, _EXTRA=extra
  plot_wcs_axes,nhdr,plot ? axsel : 10,LONARR=lonarr,LATARR=latarr,$
                GQ4=gq4,XC=xc,YC=yc,_EXTRA=extra
  
  junk = check_math()
  RETURN
  
  error_msg:
  message,'Error: Specified '+bad_ax+' is outside image bounds',/cont
  
  RETURN
  
END
