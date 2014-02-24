;+
; NAME:
;       BUILD_2D_GAUSSIAN
;
; PURPOSE:
;       Build a 2D Gaussian (with arbitrary rotation) for use with
;       prob_irdc.pro
;
; CATEGORY:
;       Distance-Omnibus Utility Routine
;
; CALLING SEQUENCE:
;       grid = BUILD_2D_GAUSSIAN( xgrid, ygrid, gauss)
;
; INPUTS:
;       XGRID -- Array of x-coordinate values (decimal degrees)
;       YGRID -- Array of y-coordinate values (decimal degrees)
;
;       GAUSS -- Array containing the gaussian parameters:
;       [0]: X  -- X position of the centroid in decimal degrees
;       [1]: Y  -- Y position of the centroid in decimal degrees
;       [2]: A  -- "Major Axis" Gaussian sigma in arcsec
;       [3]: B  -- "Minor Axis" Gaussian sigma in arcsec
;       [4]: PA -- Position angle of the major axis (degrees E of N)
;
; OPTIONAL INPUTS:
;       Optional Gauss structure member:
;       [5]: AMP -- Amplitude of the peak (Default: 1)
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       GRID  -- 2-d array of size [xgrid,ygrid] with 2-D gaussian
;                centered at [gauss.x,gauss.y]
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;       Created:  08/08/10, TPEB -- Initial version
;       Modified: 02/15/11, TPEB -- Forced double in calculations, and
;                                   corrected the calculation of the
;                                   covariance elements (there were
;                                   spurious factors of 2) -- the
;                                   factor of 2 goes in the
;                                   exponential.
;       Modified: 06/02/11, TPEB -- Documentation cleanup.
;
;-

FUNCTION BUILD_2D_GAUSSIAN, xgrid, ygrid, gauss
  
  grid = dblarr(n_elements(xgrid),n_elements(ygrid))
  
  ;; Set amplitude to 1 if not included in gauss[]
  IF n_elements(gauss) EQ 5 THEN gauss = [gauss,1.]
  
  ;; Convert gaussian sigmas into degrees
  gauss[2] /= 3600.d
  gauss[3] /= 3600.d
  
  ;; Here comes the dirty work...
  ;; Calculating the covariance elements
  a = cos(gauss[4]*!dtor)^2/gauss[3]^2 + $
      sin(gauss[4]*!dtor)^2/gauss[2]^2
  b = -sin(2.d*gauss[4]*!dtor)/2.d/gauss[3]^2 + $
      sin(2.d*gauss[4]*!dtor)/2.d/gauss[2]^2
  c = sin(gauss[4]*!dtor)^2/gauss[3]^2 + $
      cos(gauss[4]*!dtor)^2/gauss[2]^2
  
  ;; Use FOR loop to build the grid...
  FOR i=0L, n_elements(xgrid)-1 DO $
     grid[i,*] = gauss[5] * exp( -(a*(xgrid[i]-gauss[0])^2 + $
                                   2.d*b*(xgrid[i]-gauss[0])*(ygrid-gauss[1]) +$
                                   c*(ygrid-gauss[1])^2 ) / 2.d )
  
  RETURN,grid
END
