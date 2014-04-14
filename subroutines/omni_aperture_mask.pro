;+
; NAME:
;       OMNI_APERTURE_MASK
;
; PURPOSE:
;       Creates the aperture weight mask for use with the Bolocat
;       aperture photometry.  As always, it's easier to put
;       oft-used code in its own routine for access.
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       wtmask = OMNI_APERTURE_MASK( hdr, diam, l, b, [ASTR=astr] )
;
; INPUTS:
;       HDR  -- FITS header from the SURVEY image file [If ASTR is
;               used, pass an empty variable for HDR.]
;       DIAM -- Diameter (in arcseconds) of the aperture desired
;       L    -- Galactic longitude of the center of the aperture
;       B    -- Galactic latitude of the center of the aperture
;
; OPTIONAL INPUTS:
;       ASTR -- The EXTAST'd astrometry structure from the FITS
;               header from the SURVEY image file.  If passed, then
;               HDR is ignored.
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       WTMASK -- The desired weight mask for a circular aperture of
;                 diameter DIAM.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  07/22/11, TPEB -- Initial Version, code is a
;                                   modified block lifted from the
;                                   Bolocat routine
;                                   bolocat/object_photometry.pro.
;       Modified: 08/21/13, TPEB -- I really hate the ARRAY[1] <-->
;                                   SCALAR non-identicality!!!  Fixed
;                                   bug related to the new AD2XY.pro
;                                   routine in IDLASTRO.
;       Modified: 12/19/13, TPEB -- Name change to align with the
;                                   rest of the code base,
;                                   because I'm neurotic.
;                                   Also, add optional input ASTR to
;                                   override HDR.
;
;-

FUNCTION OMNI_APERTURE_MASK, hdr, diam, l, b, ASTR=astr
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; First, extract some information from the header
  IF ~n_elements(astr) THEN extast,hdr,astr
  sz = astr.naxis
  x = findgen(sz[0])#replicate(1,sz[1])
  y = replicate(1,sz[0])#findgen(sz[1])
  ad2xy,l,b,astr,xmax,ymax      ; New ad2xy spits arrays even if scalar went in
  xmax = xmax[0]                ; a 1-element array is not a scalar
  ymax = ymax[0]                ; a 1-element array is not a scalar
  dist = sqrt((x - xmax)^2 + (y-ymax)^2)
  
  ;; Now, calculate the wtmask
  rad_pix = diam / (abs(astr.cd[1,1])*3600.)/2.
  wtmask = dist LE rad_pix - 1
  border = (dist LE rad_pix + 1)-wtmask
  ind = where(border)
  xborder = ind mod sz[0]
  yborder = ind / sz[0]
  border_wt = pixwt(xmax, ymax, rad_pix, xborder, yborder) ; PIXWT from IDLASTRO
  wtmask = float(wtmask)
  wtmask[ind] = border_wt
  
  RETURN,wtmask
  
END
