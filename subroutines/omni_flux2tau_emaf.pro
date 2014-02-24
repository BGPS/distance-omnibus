;+
; NAME:
;       OMNI_FLUX2TAU_EMAF
;
; PURPOSE:
;       Calculate the conversion between SURVEY dust continuum emission
;       and the 8-micron dust optical depth (for use with GLIMPSE data).
;
; CATEGORY:
;       distance-omnibus EMAF-specific routine
;
; CALLING SEQUENCE:
;       upsilon = OMNI_FLUX2TAU_EMAF( Td [,/MJY])
;
; INPUTS:
;       Td    -- Dust temperature (scalar or vector)
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;
; KEYWORD PARAMETERS:
;       MJY   -- Output UPSILON is in unity of ** inverse mJy **
;                [Default: inverse Jy]
;
; OUTPUTS:
;       UPSILON -- The conversion factor (** in Jy^{-1} **) applied to
;                  (sub-)millimeter flux density to obtain an 8-micron
;                  optical depth.  (See Ellsworth-Bowers et al. 2013,
;                  ApJ, 770, 39; Equation 9).
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created,  03/07/11, TPEB -- Initial codified version.
;       Modified, 03/21/11, TPEB -- Clarified documentation re: vector
;                                   Td
;       Modified: 07/22/11, TPEB -- New routine for use with BGPS
;                                   (rather than IRAM) data.
;       Modified: 09/08/11, TPEB -- Moved BGPS-specific version of
;                                   UPSILON routine from
;                                   irdc_dist_model/ to utils/ and
;                                   renamed for use with the IRDC
;                                   morphological matching scheme.
;                                   Made routine a little more
;                                   functional.
;       Modified: 08/08/12, TPEB -- Updated kappa_8 with
;                                   newly-calculated value, and code
;                                   cleanup.
;       Modified: 09/06/12, TPEB -- Trying OH5 kappa_8 again -- with
;                                   new IRAC Band 4 scattering
;                                   correction.
;       Modified: 03/06/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.
;       Modified: 03/20/13, TPEB -- Added CONFFILE optional input for
;                                   conformity with other routines.
;
;-

FUNCTION OMNI_FLUX2TAU_EMAF, Td, MJY=mjy, CONFFILE=cfile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in survey-info config file
  conf = omni_load_conf(cfile)
  
  ;; Parse keywords
  units  = keyword_set( mjy ) ? 1.d3 : 1.d
  
  ;; Define dust opacities used in this conversion (cm^2 / g)
  ;; Opactiy per gram of DUST!!!
  ;; Spitzer IRAC Band 4 -- GLIMPSE
  ;;kappa_8 = 1151.d                   ; OH5 dust opactiy at 7.872um
  kappa_8 = 825.d               ; WD01 -- R=5.5A, Modified by D03
  
  Rk = kappa_8 / conf.kappa     ; Ratio of dust opacities
  
  RETURN,Rk / (planck_mm(299.792458d / (conf.nu/1.d9), Td) * units * conf.omega)
END
