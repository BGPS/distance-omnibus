;+
; NAME:
;       SET_PLOT_RANGE
;
; PURPOSE:
;       Set plot range for plotimage() routine
;
; CATEGORY:
;       Image display
;
; CALLING SEQUENCE:
;       range = set_plot_range( image [,LEVEL=level][,/INVERT] )
;
; INPUTS:
;       IMAGE  -- image array to be displayed
;
; OPTIONAL INPUTS:
;       LEVEL  -- Percentage of pixels to be mapped onto [0-255] color
;                 bits [Default: middle 99%]
;
; KEYWORD PARAMETERS:
;       INVERT -- Output plot range is for an inverted image (less ink)
;
; OUTPUTS:
;       RANGE  -- 2-element array of image values to be used for the
;                 min and max values for the plot intensity range
;
; OPTIONAL OUTPUTS:
;       NONE
;
; EXAMPLE:
;       plotrange = set_plot_range( image, level=0.97, /invert )
;
; MODIFICATION HISTORY:
;       Created:  02/10/10, TPEB -- Initial version (as get_plot_range.pro)
;       Modified: 07/09/10, TPEB -- Changed routine name for addition
;                                   into distance-omnibus
;       Modified: 10/20/10, TPEB -- Added option for inverting the
;                                   color scale
;       Modified: 01/25/11, TPEB -- Added check for infinite values in
;                                   the input image, and using only
;                                   finite values for calculating the
;                                   range.
;       Modified: 01/06/12, TPEB -- Minor changes.
;       Modified: 10/30/12, TPEB -- Added error check for univalued
;                                   images, returns [-1,0] as range.
;       Modified: 04/10/13, TPEB -- Added COMPILE_OPT statement, and
;                                   minor code clean-up.
;       Modified: 03/24/14, TPEB -- Clean up ugly code.
;-

FUNCTION SET_PLOT_RANGE, image, LEVEL=level, INVERT=invert
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Set threshold for plot range (in terms of % of pixels)
  IF ~n_elements(level) THEN level=0.99d
  thresh = (1.d - double(level)) / 2.d
  
  ;; Error check on user-supplied LEVEL
  IF (thresh GT 1.) || (thresh LT 0.) THEN BEGIN
     message,'Error: Improper LEVEL.  Returning full range.',/cont
     RETURN,minmax(image,/NAN)
  ENDIF
  
  ;; Check finiteness -- don't want to include NaN in output range
  ifin = where(finite(image),nfin)
  IF ~nfin THEN BEGIN
     message,'No finite elements in image, returning range=[0,1]',/cont
     RETURN,[0,1]
  ENDIF
  
  ;; Find lower and upper sorted indices that correspond to the threshold
  lohi = long(round([thresh*nfin, (1.d - thresh)*nfin - 1.d]))
  
  ;; Get range in one terse line.
  range = ((image[ifin])[sort(image[ifin])])[lohi]
  
  IF range[0] EQ range[1] THEN BEGIN
     message,'No dynamic range in image, returning range=[-1,0]',/inf
     RETURN,[-1,0]
  ENDIF
  
  junk = check_math()
  IF KEYWORD_SET(invert) THEN RETURN,reverse(range) ELSE RETURN,range
  
END
