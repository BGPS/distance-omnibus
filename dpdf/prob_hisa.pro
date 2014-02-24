;+
; NAME:
;       PROB_HISA
;
; PURPOSE:
;       Calculates the distance PDF for HISA based on the velocity of
;       the dense gas tracers.
;
; CATEGORY:
;       distance-omnibus
;
; CALLING SEQUENCE:
;       probability = PROB_HISA( struct [,dvec = dvec] )
;
; INPUTS:
;       STRUCT -- BGPS source structure (see READ_BGPS_CSV), which
;                 includes source longitude and latitude, and a flag
;                 which will be set for presence of IRDC
;
; OPTIONAL INPUTS:
;       DVEC   -- Vector of distances (if not specified, will build
;                 one with values from galactic_params)
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       PROBABILITY -- propbability vector as a function of distance
;       CONSTRAIN   -- Does this routine provide a constraint on the
;                      BGPS source distance (1b) or return a uniform
;                      prior (0b)?
;
; COMMON BLOCKS:
;       VEL_BLOCK
;       HI_BLOCK
;
; MODIFICATION HISTORY:
;
;       Created:  10/06/10, TPEB -- Initial Functioning Version,
;                                   following PROB_IRDC
;       Modified: 05/23/11, TPEB -- Moved calculation of (R,Z) vectors
;                                   to the new routine omni_lbd2rz.pro,
;                                   which takes into account
;                                   inaccuracies in the definition of
;                                   Galactic coordinates.
;       Modified: 12/12/13, TPEB -- Changed rotuine WEIGHT_PROB -->
;                                   OMNI_WEIGHT_DPDF to reflect new
;                                   naming convention.
;
;-

FUNCTION PROB_HISA, s, DVEC = dvec, SPECTRUM = spectrum, $ 
                    BGPS_ML = bgps_ml, HI_DIRS = hi_dirs, $
                    BGPS_DIR = bgps_dir, OBJ_DIR = obj_dir, $
                    MAP_DIR = map_dir, CONSTRAIN = constrain
  
  ;; Load the COMMON BLOCK constaining the HI structure
  COMMON VEL_BLOCK
  COMMON HI_BLOCK
  
  match = WHERE( velocity.cnum EQ s.cnum, nmatch )
  
  ;; Parse structure into useable variables
  l = s.glon_peak
  b = s.glat_peak
  
     ;; Check for the HI spectra structure
  IF size(hi,/TYPE) NE 8 THEN BEGIN            ;; Run HISA.PRO to get
                                               ;; a spectrum
     
     ;; Set directory structure, if not specified in input
     if n_elements(hi_dirs) eq 0 then $
        hi_dirs = ['./local/sgps/','./local/vgps/','./local/cgps/']
     if n_elements(bgps_dir) eq 0 then bgps_dir = './local/bgps/'
     if n_elements(obj_dir) eq 0 then obj_dir = bgps_dir+'label/'
     if n_elements(map_dir) eq 0 then map_dir = bgps_dir+'maps/'
     
     ;; Call HISA to get ourselves a spectrum
     spectrum = hisa(s, V_STD=v_std, BGPS_ML = bgps_ml, $
                     hi_dirs = hi_dirs, bgps_dir = bgps_dir,$
                     obj_dir = obj_dir, map_dir = map_dir )
     
  ENDIF ELSE BEGIN                             ;; Extract the proper spectrum
                                               ;; from the HI structure
     IF nmatch NE 1 THEN RETURN, prob
     spectrum = hi[match].spectrum
     v_std    = hi[match].v_std
     
  ENDELSE
  
  
  ;; Set defaults for the routine
  
  ;; Get galactic params
  defsysv, '!MW', exists = exists
  IF NOT exists THEN galactic_params 
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE $
     d = dindgen(!MW.NBINS)*!MW.BINSIZE + !MW.BINSTART
  R0 = !mw.r0
  
  ;; Initialize to uniform probability
  prob = fltarr(n_elements(d))+1d0/n_elements(d)  
  constrain = 0b
  
  ;; HISA is not useful for the outer galaxy.  Set uniform probability
  ;; for these longitudes.
  IF l GT 90. AND l LT 270. THEN BEGIN
     velocity[match].p_hisa = 0.
     RETURN, prob
  ENDIF
  
  ;; Perform Overlap Integral between the densegas elements and the
  ;; hisa element
  dgs = velocity[match].densegas
  
  IF TOTAL(dgs) NE 0. THEN BEGIN
     
     ;; Need to mask out HISA spectrum to only cover the range +- 5 sigma
     ;; from the center of the densegas velocity.  First, fit Gaussian
     ;; to the dense gas spectrum, then use that fit to mask out HISA...
     
     mask_sigma = 4.
     
     yfit = MPFITPEAK(velocity[match].v_std, velocity[match].densegas, A, $
                      NTERMS=3, /POSIT)
     
     window = [A[1] - mask_sigma*A[2], A[1] + mask_sigma*A[2]]
     mask = (velocity[match].v_std GE window[0]) AND $
            (velocity[match].v_std LE window[1])
     
     spectrum *= mask
     
     ;; Next, check that the masked HISA is nonzero
     IF TOTAL(spectrum) NE 0. THEN BEGIN
        
        prod = dgs * spectrum
        velocity[match].p_hisa = TOTAL(prod)*TOTAL(prod) / $
                                 (TOTAL(dgs*dgs) * TOTAL(spectrum*spectrum) )
        
     ENDIF ELSE velocity[match].p_hisa = 0.
  ENDIF ELSE velocity[match].p_hisa = 0.
  
  IF velocity[match].p_hisa GT 0. THEN BEGIN
     ;; Use the HI distribution of Wolfire et al (2003) to generate a
     ;; probability prior (a la PROB_LONLAT.PRO) as [1 - CDF].
     
     ;; Calculate (R,Z) for each of the distances, given (l,b)
     omni_lbd2rz, l, b, d, r, z
     
     ;; Get density at that R,Z -- then create the (normalized) [1 - CDF]
     rho = hi_density(r, z)
     prob = 1. - total(rho,/CUM)/total(rho)
     prob /= total(prob)
     
     ;; ;; Next, create a heaviside function with the defined width
     ;; ;; Set internal routine params
     ;; width = 200.0      ; Width of the rolloff (in pc) for the prob_vec
     ;; ;; Tangent distance
     ;; tandist = R0*cos(l*!dtor)
     ;; ;; Cut probability on this side of the tangent point; normalize
     ;; prob = 1-erf((d-tandist)/width)
     ;; prob /= total(prob)
     ;; constrain = 1b
     
     ;; Re-weight with weight
     prob = omni_weight_dpdf( prob, velocity[match].p_hisa )
     constrain = 1b
     
  ENDIF
  
  RETURN, prob
END
