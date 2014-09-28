;+
; NAME:
;       KDIST_IAU
;
; PURPOSE:
;       To return the distance to an object given l,b,v
;
; CALLING SEQUENCE:
;       dist = KDIST_IAU (L, B, V)
;
; INPUTS:
;       L   -- Galactic Longitude (decimal degrees)
;       B   -- Galactic Latitude (decimal degrees)
;       V   -- Velocity w.r.t. LSR in km/s
;
; KEYWORD PARAMETERS:
;       /NEAR, /FAR -- Report the near/far kinematic distances for Q1
;                      and Q4 data.
;       RO, VO      -- Force values for galactocentric distance for
;                      sun and velocity of the LSR around the GC.
;                      Default to 8.4 kpc and 254 km/s (Reid et al., 2009)
;       RGAL        -- Named keyword containding galactocentric radius
;                      of sources. 
;       /DYNAMICAL  -- Use the dynamical definition of the LSR
;       /KINEMATIC  -- Use the kinematic definition of the LSR (default)
;       /REGULAR    -- Do not apply the rotation correction for High
;                      mass star forming regions.
;
; OUTPUTS:
;       DIST -- the kinematic distance in units of R0 (defaults to pc).
;
; MODIFICATION HISTORY:
;
;       Fri Feb 27 00:47:18 2009, Erik <eros@orthanc.local>
;		 Adapted from kindist.pro
;       Modified: 04/20/11, TPEB -- Updated to use the GALACTIC_PARAMS
;                                   structure.
;       Modified: 04/27/11, TPEB -- Routine now gets Solar Peculiar
;                                   Motion from GALACTIC_PARAMS
;                                   structure.
;       Modified: 06/28/11, TPEB -- Documentation cleanup and
;                                   modifications to allow use of
;                                   various other rotation curves.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: made compatible with the new
;                                   framework.
;       Modified: 09/28/14, TPEB -- Correct convention that V_s is in
;                                   the direction of Galactic
;                                   rotation, and is therefore
;                                   negative for counterrotation;
;                                   change the "-" to "+".
;
;-

FUNCTION KDIST_IAU, l, b, v, NEAR=near, FAR=far, R0=r0, V0=v0, RGAL=rgal, $
                    DYNAMICAL=dynamical, KINEMATIC=kinematic, REGULAR=regular
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params config file
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  
  R0 = 8500.
  V0 = 220.
  
  IF keyword_set(regular) THEN vs = 0.0 ELSE vs=mw.VS
  
  ;; Choose a definition of the V_LSR (in RA/Dec space)
  IF (~ keyword_set(dynamical)) OR keyword_set(kinematic)  THEN BEGIN
     solarmotion_ra = ((18+03/6d1+50.29/3.6d3)*15)
     solarmotion_dec = (30+0/6d1+16.8/3.6d3)
     solarmotion_mag = 20.0d
  ENDIF ELSE BEGIN
     solarmotion_ra = ((17+49/6d1+58.667/3.6d3)*15)
     solarmotion_dec = (28+7/6d1+3.96/3.6d3)
     solarmotion_mag = 16.55294d
  ENDELSE
  
  ;; Convert l,b into RA/Dec, then find angle between V_pec and object
  euler, l, b, ra, dec, 2
  gcirc, 2, solarmotion_ra, solarmotion_dec, ra, dec, theta
  ;; Subtract off 'old' definition of LSR
  vhelio = v-solarmotion_mag*cos(theta/206265.)
  
  ;; UVW from GALACTIC_PARAMS structure
  bigu = mw.BIGU
  bigv = mw.BIGV
  bigw = mw.BIGW
  
  ;; Add in NEW definition of LSR
  vlsr = vhelio+(bigu*cos(l*!dtor)+bigv*sin(l*!dtor))*cos(b*!dtor)+$
         bigw*sin(b*!dtor)
  
  ;; This is r/r0
  null = (v0/(v0+vs)+vlsr/((v0+vs)*sin(l*!dtor)*cos(b*!dtor)))^(-1)
  
  ;;  The > 0 traps things near the tangent point and sets them to the
  ;;  tangent distance.  So quietly.  Perhaps this should pitch a flag?
  radical = sqrt(((cos(l*!dtor))^2-(1-null^2)) > 0)
  
  fardist = r0*(cos(l*!dtor)+radical)/(cos(b*!dtor))
  
  neardist = r0*(cos(l*!dtor)-radical)/(cos(b*!dtor))
  rgal = null*r0
  
  ;; If object is in Q2 or Q3, then (near = far) = far
  ind = where(abs(l-180) lt 90, ct)
  IF ct GT 0 THEN neardist[ind] = fardist[ind]
  
  IF (~ keyword_set(near)) THEN dist = fardist ELSE dist = neardist
  
  RETURN, abs(dist)
END
