;+
; NAME:
;       COLORS_FROM_VALUES
;
; PURPOSE:
;       Return a 0->255 colortable value for each value in an array
;       for plotting the "third dimension".
;
; CATEGORY:
;       Plot utility
;
; CALLING SEQUENCE:
;       plotcolors = COLORS_FROM_VALUES( array [,pr][,/LOG])
;
; INPUTS:
;       ARRAY -- Array of values.
;
; OPTIONAL INPUTS:
;       PR -- The user-specified "plot range" to override the value
;             from set_plot_range.pro.
;
; KEYWORD PARAMETERS:
;       LOG -- Use a logarithmic scaling rather than a linear one.
;
; OUTPUTS:
;       PLOTCOLORS -- The color indices in the range [0,255] that
;                     correspond to the values in ARRAY.
;
; OPTIONAL OUTPUTS:
;       PR -- The "plot range" returned by set_plot_range.pro for
;             ARRAY.
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  06/16/14, TPEB -- Initial version.
;
;-

FUNCTION COLORS_FROM_VALUES, array, pr, LOG=log
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Check keyword
  data = KEYWORD_SET(log) ? alog10(array) : array
  
  ;; Return the "plot range"
  IF ~n_elements(pr) THEN pr = set_plot_range( data )
  IF n_elements(pr) NE 2 THEN message,'Error: input pr must contain 2 elements.'
  
  ;; Make a line!
  plotcolors = fix(round((255. / (pr[1]-pr[0]) ) * (data - pr[0])))
  
  ;; Check bounds
  plotcolors = (plotcolors > 0) < 255
    
  RETURN, plotcolors
END
