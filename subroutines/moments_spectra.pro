;+
; NAME:
;       MOMENTS_SPECTRA
;
; PURPOSE:
;       Calculate the moments of a baselined spectrum (usually HHT
;       HCO+/N2H+ spectra) to be used as part of a more sophisticated
;       spectral line fitting routine.
;
; CATEGORY:
;       distance-omnibus subroutine
;
; CALLING SEQUENCE:
;       moments = MOMENTS_SPECTRA(vels, spectrum)
;
; INPUTS:
;       VELS     -- The velocities associated with the above spectrum
;       SPECTRUM -- The T_A values of the baselines spectrum
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       MOMENTS  -- Vector of the moments of the input spectrum: 
;                   [I, v_cen, v_fwhm, normalized skew, normalized
;                   kurtosis]   The last two are nomalized to the
;                   standard error in these estimators, so should only
;                   believe values at >= 3 (or so).  
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  10/07/11, TPEB -- Initial version, following recipe
;                                   from Schlingman et al. (2011).
;       Modified: 11/01/11, TPEB -- Reorder inputs to conform with
;                                   other, similar routines.
;       Modified: 06/21/12, TPEB -- Ensured DV is always positive --
;                                   some spectra run backwards!
;
;-

FUNCTION MOMENTS_SPECTRA, v, T
  
  n = n_elements(T)
  
  ;; Error catching
  IF n_elements(v) NE n THEN BEGIN
     message,'Inputs SPECTRUM and VELS must have same length.',/cont
     RETURN,0
  ENDIF
  IF n LE 1 THEN BEGIN
     message,'SPECTRUM must have n >= 2 elements.',/cont
     RETURN,0
  ENDIF
  
  ;; Initialize all moments to zero
  m0 = 0.d
  m1 = 0.d
  m2 = 0.d
  m3 = 0.d
  m4 = 0.d
  dv = abs(mean(v[1:*] - v[0:*]))
  
  ;; Loop to calculate m0 & m1 (and hence v_cen)
  FOR i=0L, n-1 DO BEGIN
     m0 += T[i] * dv
     m1 += T[i] * v[i] * dv
     m2 += T[i] * v[i] * v[i] * dv
     m3 += T[i] * v[i] * v[i] * v[i] * dv
     m4 += T[i] * v[i] * v[i] * v[i] * v[i] * dv
  ENDFOR
  
  vc = m1 / m0
  vfwhm = sqrt(8.d*alog(2.d)) * sqrt( (m2/m0 - vc*vc) > 0.d )
  varv  = (m2/m0 - vc*vc)
  skew = (m3/m0 - 3.d*vc*m2/m0 + 2.d*vc*vc*vc) / (varv)^(1.5d)
  kurt = (m4/m0 - 4.d*vc*m3/m0 + 6.d*vc*vc*m2/m0 - 3.d*vc*vc*vc*vc) / $
         (varv*varv) - 3.d
  
  ;; print,'VC^2 : ',vc*vc
  ;; print,'M2/M0: ',m2/m0
  
  RETURN,[m0,vc,vfwhm,skew/sqrt(6.d/n),kurt/sqrt(24.d/n)]
END
