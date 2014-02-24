;+
; NAME:
;       FIT_SA_SPECTRUM
;
; PURPOSE:
;       Fits a self-absorbed (2-gaussian) profile to a spectral line.
;
; CATEGORY:
;       distance-omnibus subroutine
;
; CALLING SEQUENCE:
;       yfit = FIT_SA_SPECTRUM(v, spec [,params])
;
; INPUTS:
;       V      -- Velocity scale of the spectrum
;       SPEC   -- Baselined spectrum
;
; OPTIONAL INPUTS:
;       PARAMS    -- Fit parameters to try
;       RMS       -- R.M.S. noise of the input spectrum
;       ESTIMATES -- Estimates for the outer (emission) Gaussian
;
; KEYWORD PARAMETERS:
;       QUIET  -- Passed to MPFITFUN to hide output of fitting the
;                 self-absorbed Gaussian profile and other output.
;       PLOT   -- Plot the bracket around the window used for the
;                 fit.
;
; OUTPUTS:
;       YFIT   -- Fit to the spectrum
;
; OPTIONAL OUTPUTS:
;       PARAMS -- Output fit parameters
;       ERRMSG -- Passed out from MPFITFUN
;       STATUS -- Passed out from MPFITFUN
;
; MODIFICATION HISTORY:
;       Created:  11/01/11, TPEB -- Initial Version
;       Modified: 11/08/11, TPEB -- Modified to use GAUSS2, which
;                                   follows the input pattern of
;                                   GAUSSFIT.  Also documentation
;                                   cleanup.
;-

FUNCTION FIT_SA_SPECTRUM, v, sp, p, ESTIMATES=est, RMS=rms, QUIET=quiet, $
                          ERRMSG=errmsg, STATUS=status, PLOT=plot
  
  ;; Check for input parameters, estimate from the data if no.
  IF n_elements(p) NE 6 THEN BEGIN
     
     yfit = MPFITPEAK(v,sp,A,NTERMS=3,ESTIMATES=est)
     
     IF n_elements(rms) EQ 0 THEN rcond = bytarr(n_elements(v))+1b ELSE $
        rcond = (sp GE 3.*rms)
     
     wind_size = 20.
     wind = WHERE( (v GE A[1]-wind_size AND v LE A[1]+wind_size) AND $
                   rcond, nind)
     IF nind EQ 0 THEN BEGIN
        message,'No valid data within window limits.',/cont
        RETURN,sp*0.
     ENDIF
     IF KEYWORD_SET(plot) THEN $
        plot_bracket,[A[1]-wind_size,A[1]+wind_size],0.75,$
                     thick=4,color=cgColor('TAN5')
     
     nrm = sqrt(8.d*alog(2.d))
     IF nind EQ 1 THEN mom = [A[0],A[1],A[2]*nrm] ELSE BEGIN
        mom = moments_spectra(v[wind], sp[wind])
        ;;print,'MOMENTS: ',mom[0:2]
        IF mom[2] EQ 0. THEN mom[2] = A[2]*nrm
     ENDELSE
     ;;       0        1       2          3          4            5
     ;;      PEAK    MEAN    WIDTH       PEAK      MEAN         WIDTH    
     pest = [A[0]*2.,mom[1],mom[2]/nrm,  -A[0],2.*mom[1]-A[1],mom[2]/nrm/2.]
     
     IF ~ KEYWORD_SET( quiet ) THEN BEGIN
        print,'Est Gauss1: ',pest[0:2]
        print,'Est Gauss2: ',pest[3:5]
     ENDIF     
     
  ENDIF ELSE pest = p
  
  ;; Set parameter limits
  parinfo = replicate( create_struct('value',0.d,'limited',[1b,1b],$
                                     'limits',[0.d,0.d],'parname',''), 6)
  parinfo.value = pest
  
  parinfo.parname = ['PEAK_1','MEAN_1','SIG_1 ','PEAK_2','MEAN_2','SIG_2 ']
  
  ;; Peak values (emission + absorption)
  parinfo[0].limits  = [0., 5.*max(sp)]
  parinfo[3].limits  = [-2.*max(sp),0.]
  parinfo[0].value = (parinfo[0].value < (5.*max(sp)))
  parinfo[3].value = (parinfo[3].value > (-2.*max(sp)))
  
  ;; Widths
  max_wid = 4.25
  min_wid = 0.25
  parinfo[2].limits  = [min_wid,max_wid]
  parinfo[5].limits  = [min_wid,max_wid]
  parinfo[2].value = ((parinfo[2].value < max_wid) > min_wid)
  parinfo[5].value = ((parinfo[5].value < max_wid) > min_wid)
  
  ;; V_cen
  parinfo[1].limits = [-100,200]
  parinfo[4].limits = [-100,200]
  
  p = MPFITFUN('sa_gauss',v,sp,rms_dg_spec(v,sp),PARINFO=parinfo, $ 
               QUIET=quiet, ERRMSG=errmsg, STATUS=status, MAXITER=500)
  
  yfit = sa_gauss(v,p)
  
  IF ~ KEYWORD_SET( quiet ) THEN BEGIN
     FOR j=0L,5 DO print,[parinfo[j].value,parinfo[j].limits]
     print,status,errmsg
  ENDIF  
  
  RETURN,yfit
END
