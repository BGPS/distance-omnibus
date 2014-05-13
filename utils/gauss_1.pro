;+
; NAME:
;   GAUSS_1
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute Gaussian curve given the mean, sigma and area.
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   YVALS = GAUSS1(XVALS, [PEAK, MEAN, SIGMA])
;
; DESCRIPTION:
;
;  This routine computes the values of a Gaussian function whose
;  X-values, mean, sigma, and total area are given.  It is meant to be
;  a demonstration for curve-fitting.
;
;  XVALS can be an array of X-values, in which case the returned
;  Y-values are an array as well.  The second parameter to GAUSS1
;  should be an array containing the PEAK, MEAN, and SIGMA, in
;  that order.
;
; INPUTS:
;   X - Array of X-values.
;
;   [PEAK, MEAN, SIGMA] - the peak, mean, and sigma of the 
;                         desired Gaussian curve.
;
; INPUT KEYWORD PARAMETERS:
;   NONE
;
; RETURNS:
;
;   Returns the array of Y-values.
;
; EXAMPLE:
;
;
; REFERENCES:
;
; MODIFICATION HISTORY:
;   Written, Jul 1998, CM
;   Correct bug in normalization, CM, 01 Nov 1999
;   Optimized for speed, CM, 02 Nov 1999
;   Added copyright notice, 25 Mar 2001, CM
;   Added PEAK keyword, 30 Sep 2001, CM
;   Changed p(i) -> p[i] for current convention, 8 Oct 2010, TPEB
;   Changed inputs to standard GAUSSFIT order, 8 Nov 2010, TPEB
;
;  $Id: gauss1.pro,v 1.4 2001/10/13 17:41:48 craigm Exp $
;
;-
; Copyright (C) 1998,1999,2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

FUNCTION GAUSS_1, x, p, _EXTRA=extra

  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  u = ((x-p[1])/(abs(p[2]) > 1e-20))^2
  mask = u LT (smax^2)
  norm = p[0]
  f = norm * mask * exp(-0.5*temporary(u) * mask)
  mask = 0
  
  return, f
end
