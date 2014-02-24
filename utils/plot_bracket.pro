;+
; NAME:
;       PLOT_BRACKET
;
; PURPOSE:
;       Plots a range bracket in the current graphics device.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       PLOT_BRACKET, xpositions, yposition, [IDL Graphics Keywords]
;
; INPUTS:
;       XPOSITIONS -- 2-element array describing the min & max values
;                     of the range.
;       YPOSITIONS -- Scalar describing the y-position for plot.
;
; OPTIONAL INPUTS:
;       IDL Graphics Keywords passed to OPLOT via the _EXTRA keyword.
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE (plots range bar on the current graphics device).
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  10/24/11, TPEB -- Initial version.
;
;-

PRO PLOT_BRACKET, xpos, ypos, _EXTRA=extra
  
  ;; Check sizes of input arrays
  IF n_elements(xpos) NE 2 THEN message,'Need 2-elements array xposition.',/cont
  IF n_elements(ypos) NE 1 THEN message,'Need scalar yposition.',/cont
  
  ;; Plot the horizontal part
  plots,xpos[0],ypos
  plots,xpos[1],ypos,/cont,_EXTRA=extra
  
  ;; Plot caps
  cap = (!y.crange[1] - !y.crange[0]) / 20.
  plots,xpos[0],ypos-cap/2.
  plots,xpos[0],ypos+cap/2.,/cont,_EXTRA=extra
  plots,xpos[1],ypos-cap/2.
  plots,xpos[1],ypos+cap/2.,/cont,_EXTRA=extra
  
END
