;+
; NAME:
;       SA_GAUSS
;
; PURPOSE:
;       Function to fit a self-absorbed Gaussian spectral line profile
;
; CATEGORY:
;       Mind Bender
;
; CALLING SEQUENCE:
;       yvals = SA_GAUSS( xvals, [MEAN1,SIG1,PEAK1,MEAN2,SIG2,PEAK2])
;
; INPUTS:
;       XVALS  -- x values for the fit
;       Primary (emission) Gaussian: [MEAN1, SIG1, PEAK1]
;       Secondary (absorption) Gaussian: [MEAN2, SIG2, PEAK2]
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       YVALS  -- y values of the fit
;
; OPTIONAL OUTPUTS:
;       NONE
;
; PROCEDURES USED:
;       GAUSS_1 (modified from Markwardt)
;
; MODIFICATION HISTORY:
;
;       Created:  11/01/11, TPEB -- Initial Version
;       Modified: 11/08/11, TPEB -- Modified to use GAUSS_1, which
;                                   follows the input pattern of
;                                   GAUSSFIT
;
;-

FUNCTION SA_GAUSS, x, P, _EXTRA=extra
  
  ;; Check parameter list
  IF n_elements(P) NE 6 THEN message,'Get the parameter list right!'
  
  g1 = p[0:2]
  g2 = p[3:5]
  
  f = gauss_1(x,g1) + gauss_1(x,g2)
  
  RETURN,f
END
