;+
; NAME:
;       OMNI_COMPUTE_EBARS
;
; PURPOSE:
;       Compute isoprobability error bars given a pdf of some type
;       (DPDF, mass pdf, etc.) using brute force.  Because finesse is
;       for losers.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       vals = OMNI_COMPUTE_EBARS( PDF, XVALS [,PVAL=pval] )
;
; INPUTS:
;       PDF   -- The probability density function to be analyzed.
;       XVALS -- The abscissa for PDF.
;
; OPTIONAL INPUTS:
;       PVAL -- The isoprobability level associated with the error
;               bars.  [Default: 0.6827d]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       VALS -- A 3-element array of values for this PDF: [ML value,
;               size of lower error bar, size of upper error bar]
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  02/05/14, TPEB -- Initial version, with code
;                                   extracted from
;                                   distance_omnibus.pro.
;       Modified: 02/19/14, TPEB -- Updated documentation.
;       Modified: 02/26/14, TPEB -- Make go zoom-zoom by changing loop
;                                   from a FOR to a WHILE, and going
;                                   from top to bottom, breaking when
;                                   we hit PVAL.  Also, use the values
;                                   of PDF instead of arbitrary
;                                   levels.
;
;-

FUNCTION OMNI_COMPUTE_EBARS, pdf, xvals, PVAL=pval
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Define the output variable, initialize to zero
  vals = dblarr(3)
  
  ;; Error check
  IF ~n_elements(xvals) || ~n_elements(pdf) || $
     (n_elements(pdf) NE n_elements(xvals)) THEN BEGIN
     message,'Error in input values.  Check PDF and XVALS.',/cont
     print,n_elements(pdf),n_elements(xvals)
     RETURN,vals
  ENDIF
  
  ;; Parse Keywords
  pval = ~n_elements(pval) ? 0.682689492137d : double(pval) ; Precise enough?
  
  ;;******************************************************************
  ;; Compute maximum-likelihood value
  maxp = max(pdf,maxi)          ; Peak of the function for normalization
  vals[0] = xvals[maxi]         ; MAXI indicates the ML value
  
  ;; Use brute-force method to compute the error bars 
  ;;   NOTE: Using TNMIN is unreliable for this purpose.
  ;;   ** Use the values of PDF instead of arbitrary levels! **
  lvs = reverse( pdf[sort(pdf)] ) ; Max --> Min
  totprob = TOTAL(pdf)            ; For faster computing
  ret = 0.d                       ; Start with zero area
  pp = -1L                        ; Init for WHILE loop
  WHILE ret LT pval DO BEGIN      ; Stop when the retrun fraction >= PVAL
     lind = where( pdf GE (lvs[++pp])[0], nlind)
     area = (nlind EQ 0) ? 0.d : TOTAL(pdf[min(lind):max(lind)])
     ret = area / totprob       ; Enclosed fraction of the integral
  ENDWHILE
  level = lvs[pp]               ; Highest level encl. >= PVAL
  
  ;; Error indices
  ei = WHERE(pdf GE level, nei) ; Find the edges of the region encl. by LEVEL
  IF nei EQ 0 THEN print,m4_stat(pdf)
  
  ;; [Lower:Upper] error bars
  vals[1:2] = abs( xvals[maxi] - [min(xvals[ei]),max(xvals[ei])] )
  
  RETURN, vals
END
