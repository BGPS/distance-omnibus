;+
; NAME: 
;       HI_DENSITY
;
; PURPOSE: 
;       Return the density of neutral hydrogrn (HI) in the Milky Way
;       as a function of R,Z in cylindrical coordinates.  Uses the HI
;       model from Wolfire et al. (2003, section 3.1) and vertical
;       profile from Dickey & Lockman (1990) (referenced from Wolfire 2003).
;
; CALLING SEQUENCE:  
;       rho = HI_DENSITY(r,z)
;
; INPUTS:
;       R, Z -- Galactocentric radius and height above/below the midplane
;               in pc.
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RHO -- volume mass density of neutral hydrogen in M_sun pc^-3
;
; MODIFICATION HISTORY:
;
;      Created:  02/09/11, TPEB -- Initial version; modified from
;                                  H2_DENSITY.PRO.
;      Modified: 02/14/11, TPEB -- Force double precision
;                                  calculations.
;      Modified: 03/29/11, TPEB -- Altered the vertical profile to
;                                  follow the model of Dickey &
;                                  Lockman (1990, ARA&A, 28, 215).
;
;-

FUNCTION HI_DENSITY, R, Z
  
  ;; Get galactic params
  defsysv, '!MW', exists = exists
  IF NOT exists THEN galactic_params 
  
  ;; Correction for currently-employed R0:
  R0 = !mw.r0
  corr = R0 / 8.5d3
  
  ;; Surface density (from Wolfire 2003)
  ;; Four cases (Galactocentric radius)
  c1 = r LE 4.d3 * corr
  c2 = (r GT 4.d3 * corr) AND (r LE R0)
  c3 = (r GT R0) AND (r LE 13.d3 * corr)
  c4 = (r GT 13.d3 * corr)
  
  sigma1 = 0. > (1.4d * (r/1.d3) - 0.6d)         ;; M_sun / pc^2
  sigma2 = 5.d                                   ;; M_sun / pc^2
  sigma3 = -1.12d + (6.12*r/R0)                  ;; M_sun / pc^2
  sigma4 = 8.24d * exp(-(r/1.d3 - 13.d*corr)/4)  ;; M_sun / pc^2
  
  sigma = sigma1*c1 + sigma2*c2 + sigma3*c3 + sigma4*c4
  
  ;; For the vertical profile, we use the full profile from Dickey &
  ;; Lockman, 1990, ARA&A, 28,215.
  ;; Include flaring of the disk beyond the solar circle
  flare = (1.d > exp((r-R0)/(6.7d3 * corr)) )
  
  ;; There are 3 components -- 2 Gaussian, one exponential
  ;; Note -- These are in particles per cc!
  comp1 = 0.395d * exp( -z*z *alog(2.d) / (212.d/2.d*flare)^2)
  comp2 = 0.107d * exp( -z*z *alog(2.d) / (530.d/2.d*flare)^2)
  comp3 = 0.064d * exp( -abs(z) / (403.d*flare) )
  
  ;; Now, calculate as a function of (R,z) -- M_sun / pc^3
  ;; Use the sum of the 3 components
  rho = sigma / (115.d*flare*alog(2.d + sqrt(14.d))) * (comp1+comp2+comp3)
  
  RETURN, rho
END
