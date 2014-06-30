;+
; NAME:
;       OMNI_CHECK_VERSION
;
; PURPOSE:
;       Check to see if we are running IDL >= 8.0, as required
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       OMNI_CHECK_VERSION
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE (Halts code execution if verion < 8.0)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  06/30/14, TPEB -- Initial version.
;
;-

PRO OMNI_CHECK_VERSION
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Spit error message for IDL versions earlier than 8.0
  IF fix(!version.release[0]) LT 8 THEN BEGIN
     print,''
     print,''
     message,'ERROR: The distance-omnibus code base requires IDL version '+$
             '8.0 or later to run.  Please update your installation and '+$
             'try again.'
  ENDIF
  
  RETURN
END
