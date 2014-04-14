;+
; NAME:
;       PLANCK_MM
;
; PURPOSE:
;       Evaluate the Planck function in units (and with input format)
;       relevant to milimmeter-wave astronomy.
;
; CATEGORY:
;       Astro Util
;
; CALLING SEQUENCE:
;       bbflux = PLANCK_MM( wave, temp )
;
; INPUTS:
;       WAVE   -- Scalar giving the wavelength in **MILLIMETERS** at
;                 which the Planck function is to be evaluated.
;       TEMP   -- Scalar or vector giving the temperature(s) of the
;                 Planck function to be evaluated
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       BBFLUX -- Scalar or vector giving the blackbody intensity in Jy/sr
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  02/07/11, TPEB -- Based on planck.pro from ASTROLIB
;       Modified: 09/08/11, TPEB -- Force double for all calculations,
;                                   and code cleanup.
;-

FUNCTION PLANCK_MM, wave, temp
  
  ON_ERROR,2
  
  IF n_elements(wave) EQ 0 OR n_elements(temp) EQ 0 THEN BEGIN
     message,'Syntax: bbflux = planck_mm( wave, temp )',/cont
     RETURN,0
  ENDIF
  
  ;; Constants (I *heart* MKS)
  c = 299792458d0                ;; m/s
  h = 6.6260693d-34              ;; J*s
  k = 1.3806505d-23              ;; J/K
  
  nu = c * 1.d3 / double(wave)   ;; Hz
  
  Inu = 2 * h * nu * nu * nu / c / c            ;; In W / m^2 / Hz / sr
  Inu /= exp( h * nu / k / double(temp) ) - 1.d ;; Occupancy # for a Planck gas
  
  RETURN, Inu * 1.d26                           ;; Convert to Jy
  
END
