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
;       vals = OMNI_COMPUTE_EBARS( PDF, XVALS [,NLVS=nlvs] [,PVAL=pval] )
;
; INPUTS:
;       PDF   -- The probability density function to be analyzed.
;       XVALS -- The abscissa for PDF.
;
; OPTIONAL INPUTS:
;       NLVS -- The number of levels to be used in the brute-force
;               computation of the error bars.  [Default: 1001]
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
;
;-

FUNCTION OMNI_COMPUTE_EBARS, pdf, xvals, NLVS=nlvs, PVAL=pval
  
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
  nlvs = ~n_elements(nlvs) ? 1001    : long(nlvs) 
  pval = ~n_elements(pval) ? 0.6827d : double(pval)
  
  ;;******************************************************************
  ;; Compute maximum-likelihood value
  maxp = max(pdf,maxi)          ; Peak of the function for normalization
  vals[0] = xvals[maxi]         ; MAXI indicates the ML value
  
  ;; Use brute-force method to compute the error bars 
  ;;   NOTE: Using TNMIN is unreliable for this purpose.
  lvs = findgen(nlvs)/float(nlvs-1)*max(pdf) 
  ret = fltarr(nlvs)            ; Set area return value to zero
  totprob = TOTAL(pdf)          ; For faster computing
  FOR pp=0,nlvs-1 DO BEGIN
     lind = where( pdf GE (lvs[pp])[0], nlind)
     area = (nlind EQ 0) ? 0.d : TOTAL(pdf[min(lind):max(lind)])
     ret[pp] = area / totprob   ; Enclosed fraction of the integral
  ENDFOR
  lind = where(ret GE pval, nlevel) ; Find areas >= specified PVAL
  level = nlevel EQ 0 ? max(pdf) : max(lvs[lind]) ; Highest level encl. >= PVAL
  
  ;; Error indices
  ei = WHERE(pdf GE level, nei) ; Find the edges of the region encl. by LEVEL
  IF nei EQ 0 THEN print,m4_stat(pdf)
  
  vals[1] = xvals[maxi]    - min(xvals[ei]) ; Lower error bar
  vals[2] = max(xvals[ei]) - xvals[maxi]    ; Upper error bar
  
  RETURN, vals
END
