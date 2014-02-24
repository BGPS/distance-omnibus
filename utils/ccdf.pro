;+
; NAME:
;       CCDF
;
; PURPOSE:
;       Computes the Complementary CDF of a data set P(>=x).
;
; CATEGORY:
;       Statistical Utility
;
; CALLING SEQUENCE:
;       res = CCDF( x )
;
; INPUTS:
;       X -- Data set for which the ccdf is to be computed.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       CDF -- Return regular CDF for x [P(<=x)] rather than the
;              complementary CDF.
;
; OUTPUTS:
;       RES -- The complementary CDF for x
;       X   -- The sorted version of x that goes with RES
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  03/22/13, TPEB -- Initial version.
;       Modified: 04/01/13, TPEB -- Made return value P(>=x) rather
;                                   than P(>x).  The return array
;                                   starts at unity, and the last
;                                   element is 1/n.
;       Modified: 06/11/13, TPEB -- Added /CDF keyword to return
;                                   normal CDF.
;
;-

FUNCTION CCDF, x, CDF=cdf
  COMPILE_OPT IDL2
  ON_ERROR,2
  x = x[sort(x)]
  IF KEYWORD_SET(cdf) THEN $
     RETURN, (dindgen(n_elements(x)) + 1.d) / double(n_elements(x)) ELSE $
        RETURN, 1.d - dindgen(n_elements(x)) / double(n_elements(x))
END
