;+
; NAME:
;       KDIST 
;
; PURPOSE:
;       Return the distance to an object given l,b,v.  Uses the latest
;       rotation curve from the BeSSeL collaboration, unless otherwise
;       specified as optional input.
;
; CALLING SEQUENCE:
;       dist = KDIST (L, B, V)
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
;       /KPC        -- Use kiloparsecs for output.
;
; OUTPUTS:
;       DIST -- the kinematic distance in units of R0 [Default: pc]
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
;       Modified: 06/29/11, TPEB -- Documentation cleanup and fixed
;                                   bug where the physical VLSR
;                                   replaced the input VLSR in the
;                                   code output.
;       Modified: 08/25/11, TPEB -- Added check for ALL objects
;                                   outside the Solar Circle (Q2, Q3 +
;                                   VLSR w/ no physical neardist) to
;                                   make NEAR = FAR
;       Modified: 01/23/13, TPEB -- Added /KPC keyword to return
;                                   distance in kiloparsecs.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: made compatible with the new
;                                   framework.
;
;-

FUNCTION KDIST, l, b, v, NEAR=near, FAR=far, RGAL=rgal, KPC=kpc, $
                DYNAMICAL=dynamical, KINEMATIC=kinematic, REGULAR=regular
    
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params config file
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  
  R0 = MW.R0
  V0 = MW.V0
  
  vs = keyword_set(regular) ? 0.0d : MW.VS
  
  ;; Choose a definition of the V_LSR (in RA/Dec space)
  IF ~keyword_set(dynamical) || keyword_set(kinematic) THEN BEGIN
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
  bigu = MW.BIGU
  bigv = MW.BIGV
  bigw = MW.BIGW
  
  ;; Add in NEW definition of LSR
  vlsr = vhelio+(bigu*cos(l*!dtor)+bigv*sin(l*!dtor))*cos(b*!dtor)+$
         bigw*sin(b*!dtor)
  
  ;; This is r/r0
  null = 1. / (v0/(v0-vs)+vlsr/((v0-vs)*sin(l*!dtor)*cos(b*!dtor)))
  
  ;;  The > 0 traps things near the tangent point and sets them to the
  ;;  tangent distance.  So quietly.  Perhaps this should pitch a flag?
  radical = sqrt(((cos(l*!dtor))^2-(1-null^2)) > 0)
  
  fardist  = R0*(cos(l*!dtor) + radical)/(cos(b*!dtor))
  neardist = R0*(cos(l*!dtor) - radical)/(cos(b*!dtor))
  
  Rgal = null*R0
  
  ;; If object is beyond the Solar Circle (i.e. in Q2 or Q3 OR with
  ;; unphysical near distance), then (near = far) = far
  ind = WHERE( (abs(l-180) LE 90) OR (neardist LT 0), ct)
  IF ct GT 0 THEN neardist[ind] = fardist[ind]
  
  dist = keyword_set(near) ? neardist : fardist
  
  norm = KEYWORD_SET(kpc) ? 1.d3 : 1.d
  
  RETURN, abs(dist) / norm
END
