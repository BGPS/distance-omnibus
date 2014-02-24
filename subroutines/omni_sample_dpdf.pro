;+
; NAME:
;       OMNI_SAMPLE_DPDF
;
; PURPOSE:
;       Returns a distance randomly sampled from an input DPDF.
;
; CATEGORY:
;       distance-omnibus Mass Utility
;
; CALLING SEQUENCE:
;       d = OMNI_SAMPLE_DPDF(dpdf, dvec, n)
;
; INPUTS:
;       DPDF -- Input distance probability density function
;       DVEC -- Vector of distances associated with the elements of
;               DPDF.
;
; OPTIONAL INPUTS:
;       N -- Number of random distances to return.  [Default: 1]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE
;
; OPTIONAL OUTPUTS:
;       NONE
;
; PROCEDURE:
;       From Numerical Recipes, 3rd Ed. Section 7.3.2.  The CDF of the
;       DPDF is computed.  One or more random numbers are drawn from a
;       uniform [0,1] distribution, and mapped onto distances via
;       interpolation of the CDF.
;
; MODIFICATION HISTORY:
;
;       Created:  02/13/13, TPEB -- Initial version.
;       Modified: 03/21/13, TPEB -- Name shift to OMNI_*.pro, added
;                                   COMPILE_OPT statements.
;       Modified: 04/17/13, TPEB -- Added /NAN checks in TOTAL() calls
;                                   to deal with any problems.
;
;-

FUNCTION OMNI_SAMPLE_DPDF, dpdf, dvec, n, kdar, dtan
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Error checking
  IF ~n_elements(dpdf) OR ~n_elements(dvec) THEN BEGIN
     message,'Error: Both DPDF and DVEC must be specified!',/cont
     RETURN,0b
  ENDIF
  
  IF n_elements(dpdf) NE n_elements(dvec) THEN BEGIN
     message,'Error: DPDF and DVEC must have the same length!',/cont
     RETURN,0b
  ENDIF
  
  ;; Set default n
  IF ~n_elements(n) THEN n = 1
  
  ;; Get the uniform random numbers
  rand = randomu(seed,n,/UNIFORM)
  
  ;; Apply step function if KDAR is supplied
  IF KEYWORD_SET(kdar) THEN BEGIN
     CASE kdar OF
        'N' : dpdf *= (dvec LE dtan)
        'F' : dpdf *= (dvec GE dtan)
        'T' : dpdf *= 1.d
        'U' : dpdf *= 1.d
        ELSE: BEGIN
           message,"Man, that's an f-ed up KDAR!",/cont
           RETURN,0
        END
     ENDCASE
  ENDIF
  
  ;; Compute the CDF, double-checking that the DPDF is normalized to
  ;;   unit total probability.
  cdf = total(dpdf,/cum,/nan) / total(dpdf,/nan)
  
  ;; Interpolate the random numbers onto the CDF...
  RETURN,interpol(dvec,cdf,rand)
  
END
