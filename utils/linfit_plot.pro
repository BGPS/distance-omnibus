;+
; NAME:
;       LINFIT_PLOT
;
; PURPOSE:
;       Do a linear fit to the data, and plot it on the current
;       device, with or without legend
;
; CATEGORY:
;       Plotting util
;
; CALLING SEQUENCE:
;       linfit_plot, x, y, [,/LEGEND][,/LOG][,/LINEARLOG]["Extra Keywords"]
;
; INPUTS:
;       X    -- Array of x data to be fit
;       Y    -- Array of y data to be fit
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       LEGEND    -- Add a legend with the linear fit values using
;                    LINFIT_LEGEND
;       LOG       -- Set this keyword if plot is LOG-LOG.
;       XLOG      -- The x-axis is log.
;       YLOG      -- The y-axis is log.
;       "AL_LEGEND POSITION SPECIFIERS" such as /TOP, /RIGHT or
;       POSITION which are passed on to al_legend
;
; OUTPUTS:
;       Plots the linear regression fit to the X,Y data on the current
;       plot device.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; EXAMPLE:
;       linfit_plot, x, y, /legend
;
; MODIFICATION HISTORY:
;
;       Created,  01/31/11, TPEB -- Inital Version
;       Modified: 04/05/11, TPEB -- Updated Documentation, and deleted
;                                   'POSITION' keyword (included
;                                   implicitly in _EXTRA).
;       Modified: 04/05/11, TPEB -- Added functionality for making
;                                   linar fits of LOG-LOG plots.
;       Modified: 06/16/11, TPEB -- Removed 'LINESTYLE' keyword, since
;                                   the in-routine default set was the
;                                   same as OPLOT's default
;                                   (redundant).
;       Modified: 12/12/11, TPEB -- Added XLOG and YLOG keywords to
;                                   allow for log-linear plots.
;                                   Specification of XLOG or YLOG
;                                   automatically sets /LOG.  Changed
;                                   meaning of /LOG to mean both axes
;                                   are logarithmic -- routine no
;                                   longer supports logarithmic fits.
;       Modified: 11/19/12, TPEB -- Shifting to use Coyote Graphics
;                                   System (cgOplot).
;
;-

PRO LINFIT_PLOT, x, y, LEGEND=legend, LOG=log, XLOG=xlog, YLOG=ylog, $
                 _EXTRA=extra
  
  ;; Parse keywords
  xlog = KEYWORD_SET(xlog)
  ylog = KEYWORD_SET(ylog)
  
  IF KEYWORD_SET(log) THEN BEGIN
     xlog = 1b 
     ylog = 1b
  ENDIF
  
  spread = max(!x.crange) - min(!x.crange)
  xf = findgen(101) * spread / 100. + min(!x.crange)
  
  result = LINFIT2(x,y)
  
  IF KEYWORD_SET(xlog) THEN xf = 10.^(xf)
  yf = result[0] + result[1] * xf
  
  cgOplot, xf, yf, _EXTRA=extra
  
  IF KEYWORD_SET(legend) THEN linfit_legend, result, _EXTRA=extra
  
END
