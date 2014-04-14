;+
; NAME:
;       OMNI_WEIGHT_DPDF
;
; PURPOSE:
;       De-weights a probability vector by making it closer to a
;       uniform probability without sacrificing the shape and relative
;       amplitudes of the probability peaks.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       new_prob = omni_weight_dpdf( old_prob, weight [,/VERBOSE])
;
; INPUTS:
;       old_prob  -- Original, unweighted probability from the prob_*
;                    routines
;       weight    -- Weight (in range [0,1]) to be assigned to this
;                    probability.  A weight of 0 results in a uniform
;                    probability being returned.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       VERBOSE   -- verbose output
;
; OUTPUTS:
;       new_prob  -- New, weighted probability
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  06/22/10, TPEB -- Initial version.
;       Modified: 02/27/12, TPEB -- Error handling cleanup.
;       Modified: 03/01/12, TPEB -- Internally deals with input PVEC
;                                   that is not normalized to unity
;                                   total probability (previous
;                                   implicit assumption).
;       Modified: 12/11/13, TPEB -- Brought into the modern ages, and
;                                   slight name change to be
;                                   consistent with current
;                                   noomenclature.
;
;-

FUNCTION OMNI_WEIGHT_DPDF, dpdf, weight, VERBOSE=verbose
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Error checking of input
  IF weight LT 0.d THEN BEGIN
     message,'WARNING: Input weight < 0 ... using weight = 0.',/cont
     weight = 0.d
  ENDIF
  IF weight GT 1.d THEN BEGIN
     message,'WARNING: Input weight > 1 ... using weight = 1.',/cont
     weight = 1.d
  ENDIF
  
  ;; Measure original normalization, then normalize input -- this
  ;;   allows for input DPDFs that are not normalized to unit total
  ;;   probability.
  norm = TOTAL(dpdf)
  nprob = dpdf / norm
  
  ;; Create uniform probability vector for 'dilution'
  uniform = dblarr(n_elements(nprob)) + 1.d/n_elements(nprob)
  
  ;; Create weighted probability
  new_prob = TEMPORARY(nprob) * double(weight) + $
             TEMPORARY(uniform) * (1.d - double(weight))
  
  RETURN,new_prob * norm
  
END
