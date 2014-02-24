;+
; NAME:
;       BASELINE_SPEC
;
; PURPOSE:
;       Uses a multi-sptep process to baseline spectra (mostly for use
;       with HHT spectra of HCO+ and N2H+ lines) for use with the
;       distance-omnibus project.  
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       specout = BASELINE_SPEC( specin [,ORDER] )
;
; INPUTS:
;       SPECIN  -- Input spectrum to be baselined.  Should be a 1-D array.
;
; OPTIONAL INPUTS:
;       ORDER   -- Order of the final polynomial to be subtracted
;                  [Default: 15].
;       VELS    -- Array of the velocity values associated with
;                  SPECIN.  THIS ARRAY IS REPLACED WITH THE TRIMMED
;                  VELS ARRAY SET BY VRANGE!!
;       VRANGE  -- Desired velocity range to use as baselining
;                  window.  MUST BE SPECIFIED WITH VELS!
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       SPECOUT -- Baselined spectrum
;
; OPTIONAL OUTPUTS:
;       POLYSUB -- 
;       STAGE1  -- 
;       YERROR  -- 
;       YGFIT   -- 
;       STATUS  -- 
;
; MODIFICATION HISTORY:
;
;       Created:  07/12/11, TPEB -- Initial Version
;       Modified: 08/29/11, TPEB -- Added VELS and VRANGE keywords for
;                                   specifying a baselining window for
;                                   ill-behaved data.  Documentation
;                                   cleanup.
;
;-

FUNCTION BASELINE_SPEC, specin, order, VELS=vels, VRANGE=vrange, $
                        POLYSUB=polysub, STAGE1=stage1, YERROR=yerror, $
                        YGFIT=ygfit, STATUS=status
  
  ;; Check for input ORDER
  IF n_elements( order ) EQ 0 THEN order = 20
  
  ;; Check for VELS & VRANGE
  IF n_elements( vrange ) EQ 2 THEN BEGIN
     IF n_elements( vels ) EQ 0 THEN message,$
        'Error: VELS must be specified with input VRANGE'
     vind = WHERE(vels GE vrange[0] AND vels LE vrange[1], nvind)
     IF nvind EQ 0 THEN message,$
        'Error: There are no spectrum points within VRANGE = [min,max]'
     
     ;; Trim the arrays
     specin = specin[vind]
     vels   = vels[vind]
     
  ENDIF ELSE IF n_elements( vrange ) NE 0 THEN message,$
     'Error: Input VRANGE must be [min,max]'
  
  ;; Create generic X-array, since it will not be returned [-5,5]
  x = findgen(n_elements(specin)) / float(n_elements(specin)) * 10. - 5
  
  ;; First, start with a Gaussian fit with 6 terms (including a
  ;; baseline parabola).
  parinfo = replicate({value:0., limited:[0b,0b], limits:[0.d,0.d]},6)
  parinfo[2].value   = [0.05]
  parinfo[2].limited = [0b,1b]
  parinfo[2].limits  = [0.0d,0.2d]
  
  ;; Suggestions from Craig Markwardt:
  ;; ;; 1. Get initial estimates (returned in ESTIMATES variable) without fitting
  ;; dummy = mpfitpeak(x, y, estimates, error=sigma_y, /no_fit)
  ;; ;; 2. Calculate parameter details
  ;; parinfo = ......
  ;; ;; 3. Perform actual fitting
  ;; yfit = mpfitpeak(x, y, afit, err=sigma_y, estimates=estimates,
  
  dummy = MPFITPEAK(x, specin, estimates, NTERMS=6, /NO_FIT, /POSITIVE)
  estimates[2] = parinfo[2].value
  ygfit  = MPFITPEAK(x, specin, A, NTERMS=6, PARINFO=parinfo, /POSITIVE, $
                     estimates=estimates, STATUS=status)
  
  ;; Check to see if the width of the fitted Gaussian has run up
  ;; against the limit set in parinfo.  If so, do not subtract the
  ;; Gaussian, but simply move on to the polyfit.  These cases are
  ;; likely the large-amplitude standing waves occasionally present in
  ;; the backends.
  IF A[2] EQ parinfo[2].limits[1] THEN BEGIN
     stage1 = specin 
     A = dblarr(6)
  ENDIF ELSE $
     stage1 = specin - ygfit
  
  ;; Do a polynomial fit to the subtracted data
  result = POLY_FIT(x, stage1, order, YERROR=yerror)
  
  ;; Add the coefficients from the gaussian baseline to the polyfit
  result[0] += A[3]
  result[1] += A[4]
  result[2] += A[5]
  
  ;; Create the output spectrum
  polysub = POLY(x, result)
  specout = specin - POLY(x, result)
  
  RETURN, specout
END
