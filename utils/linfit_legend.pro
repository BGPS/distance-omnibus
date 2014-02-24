;+
; NAME:
;       LINFIT_LEGEND
;
; PURPOSE:
;       Print a legend on the plot with the linear fit data
;
; CATEGORY:
;       Plotting util
;
; CALLING SEQUENCE:
;       linfit_legend, result [,LINESTYLE=linestyle]
;                             [,"al_legend position specifiers"]
;
; INPUTS:
;       RESULT    -- two-element array output from LINFIT [A,B]:(y = A + Bx)
;
; OPTIONAL INPUTS:
;       LINESTYLE -- Linestyle for linear fit line [Default=0]
;
; KEYWORD PARAMETERS:
;       LOG       -- Set this keyword if plot is LOG-LOG
;       "AL_LEGEND POSITION SPECIFIERS" such as /TOP, /RIGHT or
;       POSITION which are passed on to al_legend
;
; OUTPUTS:
;       Puts the linear fit parameters on the current plot device
;
; OPTIONAL OUTPUTS:
;       NONE
;
; EXAMPLE:
;       linfit_legend, result, /TOP, /LEFT
;
; MODIFICATION HISTORY:
;       
;       Created:  01/31/11, TPEB -- Initial Version
;       Modified: 04/05/11, TPEB -- Updated Documentation, and deleted
;                                   'POSITION' keyword (included
;                                   implicitly in _EXTRA).
;       Modified: 04/05/11, TPEB -- Added functionality for making
;                                   legends for LOG-LOG plots.
;       Modified, 12/12/11, TPEB -- Removed functionality for making
;                                   legends for LOG-LOG plots (removed
;                                   functionality in linfit_plot).
;;-

PRO LINFIT_LEGEND, result, LINESTYLE=linestyle, _EXTRA=extra
  
  IF n_elements(linestyle) EQ 0 THEN linestyle = 0
  
  ;; Examine the slope for sign
  IF result[1] GE 0 THEN pos = 1b ELSE pos = 0b
  result[1] = abs(result[1])
  
  
  IF pos THEN $
     string = 'y = '+string(result[0],format="(F0.3)")+$
              ' + '+string(result[1],format="(F0.3)")+' x' $
  ELSE $
     string = 'y = '+string(result[0],format="(F0.3)")+$
              ' - '+string(result[1],format="(F0.3)")+' x'
  
  al_legend, string, box=0, LINESTYLE=linestyle, _EXTRA=extra
  
END
