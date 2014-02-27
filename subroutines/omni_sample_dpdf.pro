;+
; NAME:
;       OMNI_SAMPLE_DPDF
;
; PURPOSE:
;       Returns a distance randomly sampled from an input DPDF.  Or,
;       more generally, returns an XVAL randomly sampled from an input
;       PDF.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       d = OMNI_SAMPLE_DPDF(pdf, xval, n)
;
; INPUTS:
;       PDF  -- Input probability density function (usually a DPDF).
;       XVAL -- Vector of x-values associated with the elements of
;               PDF.
;
; OPTIONAL INPUTS:
;       N    -- Number of random XVALs to return.  [Default: 1]
;       KDAR -- For DPDFs, may specify a KDA resolution, and the DPDF
;               will have a step function applied, with cutoff at
;               DTAN, before random XVALs are computed.
;       DTAN -- Required for use with KDAR, the tangent distance at
;               which the step function is applied.
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
;       Modified: 02/27/14, TPEB -- Some code simplification, plus
;                                   documentation update to specify
;                                   that this routine may be used more
;                                   generally than just DPDFs.
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
  
  ;; Get the uniform random numbers
  rand = randomu(seed, n_elements(n) ? n : 1, /UNIFORM)
  
  ;; Apply step function if KDAR is supplied
  IF KEYWORD_SET(kdar) THEN BEGIN
     IF n_elements(dtan) THEN BEGIN
        CASE kdar OF
           'N' : dpdf *= double(dvec LE dtan) ; Step function at d <= dtan
           'F' : dpdf *= double(dvec GE dtan) ; Step function at d >= dtan
           'T' :                              ; Tangent -- leave alone
           'O' :                              ; Outer Galaxy -- leave alone
           'U' :                              ; Unconstrained -- leave alone
           ELSE: BEGIN
              message,"Are you drunk or something?  That KDAR is FUBAR.",/cont
              RETURN,0
           END
        ENDCASE
     ENDIF ELSE $               ; End of dtan IF statement
        message,'Use of KDAR requested, but DTAN not supplied... '+$
                'Continuing without applying KDAR.',/cont
  ENDIF
  
  ;; Compute the CDF, double-checking that the DPDF is normalized to
  ;;   unit total probability.
  cdf = total(dpdf,/cum,/nan) / total(dpdf,/nan)
  
  ;; Interpolate the random numbers onto the CDF...
  RETURN,interpol(dvec,cdf,rand)
  
END
