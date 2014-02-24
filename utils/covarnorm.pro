;+
; NAME:
;       COVARNORM
;
; PURPOSE:
;       Normalizes the covariance matrix into the correlation matrix
;
; CATEGORY:
;       Utils
;
; CALLING SEQUENCE:
;       corr = COVARNORM( covar )
;
; INPUTS:
;       COVAR -- The covariance matrix
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       CORR -- The correlation matrix
;
; OPTIONAL OUTPUTS:
;       NONE
;
; NOTES:
;       This routine was downloaded from https://setisvn.ssl.berkeley.edu/
;       trac/browser/galfa/gsr2.7/phil/gen/covarnorm.pro 
;
; MODIFICATION HISTORY:
;
;       Added:    08/17/11, TPEB -- Initial codified version
;
;-

FUNCTION COVARNORM, covar
  
  s=size(covar)
  doug=covar[indgen(s[1])*(s[1]+1)]
  doug = doug#doug
  RETURN,covar/sqrt(abs(doug))
  
END
