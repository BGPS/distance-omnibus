;+
; NAME:
;       ARRSTEP
;
; PURPOSE:
;       Compute the step size in a regularly-sampled array (i.e. the
;       XARR from plothist).
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       dx = ARRSTEP( array )
;
; INPUTS:
;       ARRAY -- The regularly-sampled array for which step size is to
;                be computed.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NAN -- Treat the array for NaN before computing.
;
; OUTPUTS:
;       DX -- The desired step size for the array.  Computed as the
;             median of all the steps in the array.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  01/08/14, TPEB -- Initial version -- got sick of
;                                   typing the requisite code.
;
;-

FUNCTION ARRSTEP, array, NAN=nan
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Clean NaN?
  IF KEYWORD_SET(nan) THEN BEGIN
     fin = where(finite(array),nfin)
     IF nfin THEN array = array[fin] ELSE BEGIN
        message,'Warning: input array has no finite elements... '+$
                'returning zero.',/cont
        RETURN,0
     ENDELSE
  ENDIF
  
  RETURN, median(array[1:*] - array[0:*])
END
