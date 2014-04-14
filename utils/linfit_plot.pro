;+
; NAME:
;       LINFIT_PLOT
;
; PURPOSE:
;       Do a linear fit to the data, and plot it on the current
;       device, with or without legend.
;
; CATEGORY:
;       Plotting utility
;
; CALLING SEQUENCE:
;       linfit_plot, x, y, [,/ROBUST][,/LEGEND][,/LOG]["Extra Keywords"]
;
; INPUTS:
;       X    -- Array of x data to be fit
;       Y    -- Array of y data to be fit
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       ROBUST    -- Use an outlier-robust linear fit routine instead
;                    of a least-squares scheme.
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
;       RES -- [a,b], where y = a + b*x
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
;       Modified: 03/10/14, TPEB -- Added ROBUST keyword to utilize
;                                   the IDL function LADFIT to fit an
;                                   outlier-robust line to the data.
;                                   Some documentation cleanup.  Also
;                                   add RES optional output to allow
;                                   return of the fit values.
;
;-

PRO LINFIT_PLOT, x, y, LEGEND=legend, LOG=log, XLOG=xlog, YLOG=ylog, $
                 ROBUST=robust, RES=result, _EXTRA=extra
  
  ;; Parse keywords
  xlog = KEYWORD_SET(xlog)
  ylog = KEYWORD_SET(ylog)
  
  IF KEYWORD_SET(log) THEN BEGIN
     xlog = 1b 
     ylog = 1b
  ENDIF
  
  ;; Compute x-range of the plot
  spread = max(!x.crange) - min(!x.crange)
  xf = findgen(101) * spread / 100. + min(!x.crange)
  
  ;; Do the linear fit
  result = KEYWORD_SET(robust) ? LADFIT(x,y,/DOUBLE) : LINFIT2(x,y)
  
  IF KEYWORD_SET(xlog) THEN xf = 10.^(xf)
  yf = result[0] + result[1] * xf
  
  ;; Plot!
  cgOplot, xf, yf, _EXTRA=extra
  
  ;; If LEGEND is specified, call with accumulated keywords
  IF KEYWORD_SET(legend) THEN linfit_legend, result, _EXTRA=extra
  
END
