;+
; NAME:
;       ONE2ONE
;
; PURPOSE:
;       Draw a dashed diagonal line on a plot along the 1:1 relationship.
;
; CATEGORY:
;       Plotting utilities
;
; CALLING SEQUENCE:
;       ONE2ONE
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       Routine accepts usual graphics keywords and passes them to
;       CGPLOTS via _EXTRA parameter.
;
; KEYWORD PARAMETERS:
;       LOG -- This is a log-log plot.  NOTE: This routine will not
;              compute the CURVED 1:1 line for log-linear plots!
;
; OUTPUTS:
;       NONE (draws a line on the current plot device)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  11/20/12, TPEB -- Initial Version
;       Modified: 06/20/13, TPEB -- Added LOG keyword for log-log
;                                   plots, and added COMPILE_OPT and
;                                   ON_ERROR statements.
;
;-

PRO ONE2ONE, LOG=log, _EXTRA=extra
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ON_ERROR, 2
  
  log = KEYWORD_SET(log)
  
  ;; Find the lower-left corner
  ll = !y.crange[0] > !x.crange[0]
  
  ;; Fine the upper-right corner
  ur = !y.crange[1] < !x.crange[1]
  
  IF log THEN BEGIN
     ll = 10.^ll
     ur = 10.^ur
  ENDIF
  
  cgPlots,[ll,ur],[ll,ur],_EXTRA=extra
  
END
