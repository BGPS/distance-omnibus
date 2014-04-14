;+
; NAME:
;       OMNI_KDIST_SPECTRUM
;
; PURPOSE:
;       Calculates a DPDF for a source given (l,b,v_spec).  Designed
;       to be a modular insert for prob_kidst and prob_grsmatch.
;
; CATEGORY:
;       distance-omnibus Rotation Curve Subroutine
;
; CALLING SEQUENCE:
;       dpdf = OMNI_KDIST_SPECTRUM(l, b, v_std, v_spec [,DVEC = dvec]
;                                  [,CONSTRAIN=constrain])
;
; INPUTS:
;       L      -- Galactic Longitude
;       B      -- Galactic Latitude
;       V_STD  -- Velocity values
;       V_SPEC -- Velocity spectrum (derived from velocity structure,
;                 or otherwise)
;
; OPTIONAL INPUTS:
;       DVEC -- Distance vector for computation
;               [Defualt: derived from dpdf_params.conf]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       DPDF -- Distance Probability Density Function
;
; OPTIONAL OUTPUTS:
;       CONSTRAIN -- The constraint bit to be passed back up to the
;                    calling function.
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;       Created:  10/03/10, TPEB -- Initial Version to replace
;                                   identical code blocks in prob_kdist
;                                   and prob_grsmatch
;       Modified: 04/27/11, TPEB -- Clarified documentation, and
;                                   offloaded the conversion from
;                                   physical radial velocity to V_LSR
;                                   (as reported by telescopes) to a
;                                   new routine
;                                   (utils/vphys2vlsr.pro).
;       Modified: 04/04/12, TPEB -- Added pass-through keywords /CLEM
;                                   and /IAU for rotation curve
;                                   variants -- passed to VPHYS2VLSR.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.  Moved input options
;                                   ERRVLSR, CLEM, IAU to config
;                                   files.
;
;-

FUNCTION OMNI_KDIST_SPECTRUM, l, b, v_std, v_spec, DVEC=dvec, $
                              CONSTRAIN=constrain
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params & dpdf-params config files
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  
  ;; Set uniform probability in case of early return
  prob = dblarr(dpdfs.nbins) + 1.d0/dpdfs.nbins
  constrain = 0b
  
  ;; Check that v_spec is not null...
  IF TOTAL(v_spec) EQ 0 THEN RETURN, prob
  
  ;; If v_spec is nonzero, then calculate!
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE BEGIN
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
     dvec = d
  ENDELSE
  
  ;; Use the routine VPHYS2VLSR to get the array of the VLSR that a
  ;; telescope would report (VLSR for each of the distances in the
  ;; dvec array).  This code block was moved to a new routine for
  ;; ease of use by other programs.
  vlsr = omni_vphys2vlsr(l, b, d)
  
  ;; Integrate the product of v(\dsun) * v_spec over velocity
  prob *= 0.d                   ; Start w/ null prob 
  FOR q = 0L, n_elements(v_std)-1 DO BEGIN
     prob = prob + exp( -(vlsr-v_std[q])*(vlsr-v_std[q]) / $
                        (2.d*mw.errvlsr*mw.errvlsr) ) * v_spec[q]
  ENDFOR
  
  ;; Normalize to unit integrated probability & return
  constrain = 1b
  RETURN, prob / TOTAL(prob)
  
END
