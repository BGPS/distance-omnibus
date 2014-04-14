;+
; NAME:
;       OMNI_LEAD_CONF
;
; PURPOSE:
;       Loads the correct conf structure based on the desired
;       survey-info configuration file.  Replaces identical code
;       blocks in many OMNI_*.pro routines.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       conf = OMNI_LOAD_CONF( cfile )
;
; INPUTS:
;       CFILE -- Optional input from calling routine for the
;                survey-info configuration file.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       CONF -- IDL structure containing the information from the
;               survey-info configuration file.
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
;       Created:  03/21/13, TPEB -- Initial version.
;       Modified: 08/20/13, TPEB -- Streamlined.
;
;-

FUNCTION OMNI_LOAD_CONF, cfile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Check if cfile was specified in the calling routine -OR- 
  ;;       if conffile has not previously been defined 
  IF n_elements(cfile) || ~n_elements(conffile) THEN BEGIN
     conffile = n_elements(cfile) ? cfile : './conffiles/survey_info.conf'
     conf = omni_read_conffile(conffile)
  ENDIF
  
  RETURN, conf                  ; Return existing conf if no new one read in
END
