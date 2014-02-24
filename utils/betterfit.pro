;+
; NAME:
;       BETTERFIT
;
; PURPOSE:
;       Creates a 2D Gaussian
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       z = BETTERFIT( x, y, p )
;
; INPUTS:
;       X -- 2D array whose values are equal to their x position
;       Y -- 2D array whose values are equal to their y position
;       P -- 6 element parameter array
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       Z -- 2D array whose values are the desired Gaussian
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  xx/xx/xx, UNKNOWN -- Function was part of code
;                                      retrieved from
;                                      signals.siglab.ok.ubc.ca as
;                                      part of the GLIMPSE processing
;                                      code base.
;       Modified: 12/21/11, TPEB -- Added documentation.
;
;-

FUNCTION BETTERFIT, x, y, p
  
  RETURN, p[1] * exp(-0.5*((x-p[4])/p[2])^2-0.5*((y-p[5])/p[3])^2) + p[0]
  
END
