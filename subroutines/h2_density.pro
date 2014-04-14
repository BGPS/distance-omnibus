;+
; NAME: 
;       H2_DENSITY
;
; PURPOSE: 
;       Return the density of H2 in the Milky Way as a function of R,Z
;       in cylindrical coordinates.  Uses the prescription from
;       Wolfire at el. (2003) for a single-Gaussian model of H2
;       outside 0.2 R0.  Optionally can calculate the H2 density for a
;       new double-gaussian model fit to the Bronfman (1988) CO data
;       for the Northern Galactic Plane.  NOTE: All values scaled to
;       R0 of Reid (2009). 
;
; CATEGORY:
;       distance-omnibus Physical Property routine
;
; CALLING SEQUENCE:  
;       rho = H2_DENSITY(r,z [,/DGAUSS])
;
; INPUTS:
;       R, Z -- Galactocentric radius and height above/below the midplane
;               in pc.
;
; KEYWORD PARAMETERS:
;       DGAUSS -- Optionally calculate the H2 density using the new
;                 double-Gaussian fit to the Bronfman (1988) data.
;
; OUTPUTS:
;       RHO -- volume mass density of molecular hydrogen [M_sun/pc^3]
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Modified: 09/07/10, TPEB -- Renamed the routine, and made all
;                                   galactocentric distances scalable
;                                   by the current version of R0.
;       Modified: 09/27/10, TPEB -- Changed the model fit to a
;                                   double-gaussian fit to just the
;                                   Northern plane of Bronfman 1988.
;       Modified: 02/09/11, TPEB -- Made sure all calculations are in
;                                   double-precision (for good
;                                   measure).
;       Modified: 02/06/12. TPEB -- Put back in the Wolfire et al.
;                                   (2003) single-Gaussian fit as the
;                                   default, with the fancy
;                                   double-Gausisan returnable as an
;                                   option.
;       Modified: 03/06/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: made compatible with the
;                                   new framework.
;
;-

FUNCTION H2_DENSITY, R, Z, DGSS=dgauss
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  
  R0 = mw.r0
  sigfwhm = sqrt(8.d * alog(2.d))
  
  IF keyword_set( dgauss ) THEN BEGIN
     ;; Fiddle-able parameters
     ;; Double-Gaussian Fit to the Bronfman (1988) Data
     
     ;; Surface Density
     sden_0        = 0.058d      ; \pm 0.01 M_sun / pc^2
     A1            = 6.15d       ; \pm 0.7  M_sun / pc^2
     R1            = 0.527d * R0 ; \pm 0.03 R0
     sig1          = 0.105d * R0 ; \pm 0.02 R0
     A2            = 2.61d       ; \pm 0.9  M_sun / pc^2
     R2            = 0.773 * R0  ; \pm 0.05 R0
     sig2          = 0.075 * R0  ; \pm 0.05 R0
     den_sc        = 1.4d        ; M_sun / pc^2   at Solar Circle
     outer_sl      = 0.29d * R0  ; \pm 0.02
     trans         = 0.761d * R0 ; pc  Transition from inner model to outer 
     ;; Vertical Scale Heights
     inner_g_hwhm  = 59.d                      ;; pc
     outer_g_scale = 6.7d3 * R0 / 8.5d3        ;; Scaled value to new R0
     
     ;; Now, calculate as a function of (R,z)
     ;; Scale Height
     scaleht = inner_g_hwhm/sqrt(2.d*alog(2.d)) * $
               (1.d > exp((r-R0)/(outer_g_scale)) )
     
     ;; Case 1 -- Within Transition Point
     sd1 = sden_0 + A1 * exp( -(r-R1)^2 / (2. * sig1^2)) + $
           A2 * exp(-(r-R2)^2 / (2. * sig2^2))
     rho1 = sd1/sqrt(2.*!dpi)/scaleht*exp(-z^2/(2.*scaleht^2))
     
     ;; Case 2 -- Outside Transition Point
     sd2 = den_sc * exp(-(r-R0)/outer_sl)
     rho2 =  sd2/sqrt(2.*!dpi)/scaleht*exp(-z^2/(2.*scaleht^2))
     
     rho = (r le trans)*rho1+(r gt trans)*rho2
     
  ENDIF ELSE BEGIN
     
     ;; Parameters from Wolfire et al. (2003) for single-Gaussian fit
     WA1 = 4.5d                         ; M_sun / pc^2
     WR1 = 0.571d * R0                  ; pc
     Wsl = 0.34d * R0                   ; pc
     Wtr = 0.82d * R0                   ; pc
     Wsig = 0.52d * R0 / sigfwhm        ; pc
     ;; Vertical Scale Heights
     den_sc        = 1.4d               ; M_sun / pc^2   at Solar Circle
     inner_g_hwhm  = 59.d               ; pc
     outer_g_scale = 6.7d3 * R0 / 8.5d3 ; Scaled value to new R0
     
     ;; Now, calculate as a function of (R,z)
     ;; Scale Height
     scaleht = inner_g_hwhm/sqrt(2.d*alog(2.d)) * $
               (1.d > exp((r-R0)/(outer_g_scale)) )
     
     ;; Case 1 -- Within Transition Point
     sd1  = WA1 * exp(-((r-WR1)*(r-WR1)) / (2.d* Wsig*Wsig ))
     rho1 = sd1/sqrt(2.*!dpi)/scaleht*exp(-z^2/(2.*scaleht^2))
     
     ;; Case 2 -- Outside Transition Point
     sd2 = den_sc * exp(-(r-R0)/Wsl)
     rho2 =  sd2/sqrt(2.*!dpi)/scaleht*exp(-z^2/(2.*scaleht^2))
     
     rho = (r le Wtr)*rho1+(r gt Wtr)*rho2
     
  ENDELSE
  
  RETURN, rho
END
