;+
; NAME:
;       PLOT_WCS AXES
;
; PURPOSE:
;       Plot the WCS coordinates around a plotimage plot
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       PLOT_WCS_AXES, HEADER [,SELECT][,LONARR=lonarr][,LATARR=latarr]
;
; INPUTS:
;       HEADER   -- FITS header for the image 
;
; OPTIONAL INPUTS:
;       SELECT   -- Names to be given to the coordinate axes [default = 1]
;                   1)    Galactic Coordinates  (l,b)  (tickformat="F0.2")
;                   2)    Celectial Coordinates (R.A., Dec.)
;                   3)    Galactic Coordinates  (l,b)  (tickformat="I0")
;                   6)    Galactocentric Position [kpc]
;                   7)    L-V Plot (GLON, VLSR)
;                   8)    IGPS (b,v) slices (v, b)
;                   9)    L-dsun plots
;                   10)   NO PLOT -- ONLY create LONARR & LATARR
;                   else) "Unknown"
;       AXCOLOR  -- Name of the color to use for the axes.  
;                   [Default: 'Opposite'] 
;
; KEYWORD PARAMETERS:
;       GQ4 -- Galactic Quadrant IV; label longitude with 180-360 deg
;              instead of negative values.
;
; OUTPUTS:
;       Plots WCS axes on the current graphics device (unless SELECT=10)
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
;
; COMMON BLOCKS:
;       NONE
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;       Created:  08/27/10, TPEB -- Initial Version
;       Modified: 10/21/10, TPEB -- Added select option for no plot
;       Modified: 01/25/11, TPEB -- Added documentation
;       Modified: 04/18/11, TPEB -- Edited documentation to inlude
;                                   info about no plot option
;                                   (SELECT=10)
;       Modified: 06/01/11, TPEB -- Added pass-through _EXTRA input
;                                   for graphical keywords to be
;                                   passed on to axis.
;       Modified: 06/23/11, TPEB -- Added SELECT = 8 for IGPS (b,v)
;                                   slices.
;       Modified: 11/04/11, TPEB -- Force axis colors to be 'Opposite'
;                                   (using cgColor()) regardless of
;                                   the loaded color table.
;       Modified: 11/10/11, TPEB -- Cleaned up the axis color code
;                                   block, calling cgColor() only
;                                   once.
;       Modified: 01/23/12, TPEB -- Added _EXTRA pass-through to 2
;                                   remaining axis calls -- needed to
;                                   pass XTHICK & YTHICK inputs.
;       Modified: 08/02/12, TPEB -- Added AXCOLOR as input, and moved
;                                   axis --> cgAxis
;       Modified: 10/24/12, TPEB -- Finally corrected LONARR & LATARR
;                                   to be one element longer than the
;                                   input image, so that the elements
;                                   of these arrays correspond to the
;                                   EDGES of the pixels in the input
;                                   image.  Also finally fixed XC & YC
;                                   arrays.  Now, contours should
;                                   actually line up correctly.  Also
;                                   cleaned up documentation.
;       Modified: 11/27/12, TPEB -- Fixed how AXCOLOR is implemented,
;                                   so that the outer border is still
;                                   'Opposite', and only the ticks are
;                                   in AXCOLOR.
;
;-

PRO PLOT_WCS_AXES, hdr, SELECT, LONARR=lonarr, LATARR=latarr, XC=xc, $
                   YC=yc, GQ4=gq4, AXCOLOR=axcolor, _EXTRA=extra
  
  select = (n_elements(select) EQ 0) ? 1b : byte(select)
  IF n_elements(axcolor) EQ 0 THEN axcolor = 'Opposite'
  
  ;; Set coordinate system
  CASE select OF
     1: BEGIN
        xtitle = 'Galactic Longitude [deg]'
        ytitle = 'Galactic Latitude [deg]'
        xtf = "(F0.2)"
        ytf = "(F0.2)"
     END
     2: BEGIN
        xtitle = 'Right Ascension [deg]'
        ytitle = 'Declination [deg]'
        xtf = "(F0.2)"
        ytf = "(F0.2)"
     END
     3: BEGIN
        xtitle = 'Galactic Longitude [deg]'
        ytitle = 'Galactic Latitude [deg]'
        xtf = "(I0)"
        ytf = "(I0)"
     END
     6: BEGIN
        ytitle = 'Galactocentric Position [kpc]'
        xtitle = 'Galactocentric Position [kpc]'
        ytf = "(I0)"
        xtf = "(I0)"
     END
     7: BEGIN
        ytitle = 'V!dLSR!n [km s!u-1!n]'
        xtitle = 'Galactic Longitude [deg]'
        ytf = "(I0)"
        xtf = "(I0)"
     END
     8: BEGIN
        xtitle = 'V!dLSR!n [km s!u-1!n]'
        ytitle = 'Galactic Latitude [deg]'
        xtf = "(I0)"
        ytf = "(F0.2)"
     END
     9: BEGIN
        ytitle = 'Heliocentric Distance [kpc]'
        xtitle = 'Galactic Longitude [deg]'
        ytf = "(I0)"
        xtf = "(I0)"
     END
     ELSE: BEGIN
        xittle = 'Unknown'
        ytitle = 'Unknown'
     END
  ENDCASE
  
  ;; Extract astrometry
  extast,hdr,astr
  
  ;; Build LONARR & LATARR
  IF astr.cd[1,1] EQ 1 THEN $
     cd = astr.cdelt ELSE $
        cd = [astr.cd[0,0],astr.cd[1,1]]
  
  ;; Check for GQ4 Keyword
  IF KEYWORD_SET( gq4 ) THEN astr.crval[0] += 360.
  
  
  ;; Generate the LONARR & LATARR arrays
  lonarr = cd[0] * (findgen(astr.naxis[0]+1) - astr.crpix[0] + 0.5) + $
           astr.crval[0]
  latarr = cd[1] * (findgen(astr.naxis[1]+1) - astr.crpix[1] + 0.5) + $
           astr.crval[1]
  
  ;; Generate XC & YC arrays for contour plots
  xc = cd[0] * (findgen(astr.naxis[0]) - astr.crpix[0] + 1.0) + astr.crval[0]
  yc = cd[1] * (findgen(astr.naxis[1]) - astr.crpix[1] + 1.0) + astr.crval[1]
  
  ;; Plot axes -- UNLESS SELECT=10
  IF select EQ 10 THEN RETURN
  
  cgAxis,xr=[lonarr[0],lonarr[astr.naxis[0]]],color='Opposite',$
         /xst,/sav,xaxis=0,xtitle=xtitle,xtickformat=xtf, _EXTRA=extra
  cgAxis,color='Opposite',/xst,xaxis=1,xtickformat='blank_axis',$
         xtitle='', _EXTRA=extra
  cgAxis,yr=[latarr[0],latarr[astr.naxis[1]]],color='Opposite',$
         /yst,/sav,yaxis=0,ytitle=ytitle,ytickformat=ytf, _EXTRA=extra
  cgAxis,color='Opposite',/yst,yaxis=1,ytickformat='blank_axis',$
         ytitle='', _EXTRA=extra
  
  IF ~ STRCMP(strtrim(axcolor,2),'Opposite',/FOLD_CASE) THEN BEGIN
     cgAxis,xr=[lonarr[0],lonarr[astr.naxis[0]]],color=axcolor,$
            /xst,/sav,xaxis=0,xtickformat='blank_axis', _EXTRA=extra
     cgAxis,color=axcolor,/xst,xaxis=1,xtickformat='blank_axis',$
            _EXTRA=extra
     cgAxis,yr=[latarr[0],latarr[astr.naxis[1]]],color=axcolor,$
            /yst,/sav,yaxis=0,ytickformat='blank_axis', _EXTRA=extra
     cgAxis,color=axcolor,/yst,yaxis=1,ytickformat='blank_axis',$
            _EXTRA=extra
     
     cgAxis,color='Opposite',xaxis=0,xtickformat='blank_axis', $
            ticklen=0, _EXTRA=extra
     cgAxis,color='Opposite',xaxis=1,xtickformat='blank_axis',$
            ticklen=0, _EXTRA=extra
     cgAxis,color='Opposite',yaxis=0,ytickformat='blank_axis', $
            ticklen=0, _EXTRA=extra
     cgAxis,color='Opposite',yaxis=1,ytickformat='blank_axis', $
            ticklen=0, _EXTRA=extra
     
  ENDIF

  junk = check_math()
  RETURN
END
