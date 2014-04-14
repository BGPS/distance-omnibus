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
;       LOGCUT -- For pdfs that span several orders of magnitude, the
;                 binning choice may effect relative peaks of bimodal
;                 distributions; this option CUTS the pdf to consider
;                 only the peak with the majority of the probability.
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
;       Modified: 03/07/14, TPEB -- Add LOGCUT keyword to deal with
;                                   bimodal pdfs where log-vs-linear
;                                   binning plays havoc.
;
;-

FUNCTION OMNI_COMPUTE_EBARS, pdf, xvals, PVAL=pval, LOGCUT=logcut, $
                             XVMIN=xvmin, NJ=nj
  
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
  pval   = ~n_elements(pval) ? 0.682689492137d : double(pval) ; Precise enough?
  
  
  ;;******************************************************************
  ;; Apply LOGCUT, if desired
  IF KEYWORD_SET(logcut) THEN BEGIN
     
     ix = indgen(n_elements(pdf))
     mp = max(pdf)
     i4 = where(pdf GE mp / 4.d, n4) ; Indices where PDF >= 1/4 MAX
     jump = (i4[1:*] - i4[0:*]) NE 1 ; Steps between indices, >1 == JUMP
     ij = where(jump, nj)            ; Jump index -- break at ij -> ij+1
     
     print,nj
     ppdf = pdf                              ; Set DEFAULT in case NJ == 0
     FOR jj=0,nj-1 DO BEGIN                  ; Only do if nj >= 1
        igap   = ix[i4[ij[jj]]:i4[ij[jj]+1]] ; Indices of PDF in the gap
        gap    = pdf[igap]                   ; Y-VALS of the GAP
        xvs    = xvals[igap]                 ; X-VALS of the GAP
        mingap = min(gap,ixv)                ; Minimum value in the GAP
        
        IF mingap LE 0.1d * mp THEN BEGIN ; Skip if saddle >= 0.1 x peak
           xvmin = xvs[ixv]               ; X-VAL of the minumum GAP value
           
           ;; Compute integrated probs
           totprob = TOTAL(pdf) ; For faster computing
           plo = total(pdf * double(xvals LE xvmin)) / totprob
           phi = total(pdf * double(xvals GT xvmin)) / totprob
           
           ;; Deep down in here is the point to this whole exercise...
           CASE 1 OF
              plo GE 0.6: ppdf = pdf * double(xvals LE xvmin) ; MASK
              phi GE 0.6: ppdf = pdf * double(xvals GT xvmin) ; MASK
              ELSE      : ppdf = pdf                          ; ELSE leave
           ENDCASE
        ENDIF ELSE ppdf = pdf   ; ELSE leave alone
     ENDFOR
  ENDIF ELSE ppdf = pdf         ; ELSE leave alone
  
  
  ;;******************************************************************
  ;; Compute maximum-likelihood value
  maxp = max(ppdf,maxi)         ; Peak of the function for normalization
  vals[0] = xvals[maxi]         ; MAXI indicates the ML value
  
  ;; Use brute-force method to compute the error bars 
  ;;   NOTE: Using TNMIN is unreliable for this purpose.
  ;;   ** Use the values of PPDF instead of arbitrary levels! **
  lvs = reverse( ppdf[sort(ppdf)] ) ; Max --> Min
  totprob = TOTAL(ppdf)             ; For faster computing
  ret = 0.d                         ; Start with zero area
  pp = -1L                          ; Init for WHILE loop
  WHILE ret LT pval DO BEGIN        ; Stop when the retrun fraction >= PVAL
     lind = where( ppdf GE (lvs[++pp])[0], nlind)
     area = (nlind EQ 0) ? 0.d : TOTAL(ppdf[min(lind):max(lind)])
     ret = area / totprob       ; Enclosed fraction of the integral
  ENDWHILE
  level = lvs[pp]               ; Highest level encl. >= PVAL
  
  ;; Error indices
  ei = WHERE(ppdf GE level, nei) ; Find the edges of the region encl. by LEVEL
  IF nei EQ 0 THEN print,m4_stat(ppdf)
  
  ;; [Lower:Upper] error bars
  vals[1:2] = abs( xvals[maxi] - [min(xvals[ei]),max(xvals[ei])] )
  
  undefine,ppdf                 ; Clean up after myself
  RETURN, vals
END
