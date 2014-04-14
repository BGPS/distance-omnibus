;+
; NAME:
;       OMNI_VSPEC
;
; PURPOSE:
;       Takes the VLSR, LW, and (optionally) the TMB of a velocity
;       spectrum fit and returns a velocity spectrum, with v_std
;       values, based on survey_info.conf.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       spec = OMNI_VSPEC(A, v_std)
;
; INPUTS:
;       A -- The 3-element array [TMB,VLSR,LW] describing the Gaussian
;            fit to a spectral line, as would be returned from the
;            function GAUSSFIT. 
;
; OPTIONAL INPUTS:
;       V_STD -- The abscissa vector used to generate SPEC.
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       SPEC -- The velocity spectrum, as returned by the function
;               GAUSS2.pro.
;
; OPTIONAL OUTPUTS:
;       V_STD -- The abscissa vector used to generate SPEC.
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  02/28/13, TPEB -- Initial version.
;       Modified: 03/20/13, TPEB -- Added CONFFILE optional input for
;                                   conformity with other routines.
;
;-

FUNCTION OMNI_VSPEC, A, v_std, CONFFILE=cfile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in the configuration file
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN,0
  ENDIF
  
  IF n_elements(v_std) EQ 0 THEN $
     v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
  
  RETURN, gauss2(v_std,A)
  
END
