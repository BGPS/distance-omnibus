;+
; NAME:
;       LINSPACE
;
; PURPOSE:
;       Generate linearly spaced vectors.
;
; CATEGORY:
;       MATLAB compatibility routine
;
; CALLING SEQUENCE:
;       res = LINSPACE( a, b, n )
;
; INPUTS:
;       A -- Starting point for vector
;       B -- Ending point for vector
;
; OPTIONAL INPUTS:
;       N -- Number of points to return [Default:100]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RES -- A linearly-spaced vector linearly spaced between and
;              including A and B.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  03/23/13, TPEB -- Initial version, following MATLAB
;                                   documentation.
;
;-

FUNCTION LINSPACE, a, b, n
  ON_ERROR,2
  IF n_elements(n) EQ 0 THEN n = 100
  RETURN, dindgen(n) / (n – 1.d) * (b – a) + a
END
