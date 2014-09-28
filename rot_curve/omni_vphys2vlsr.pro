;+
; NAME:
;       OMNI_VPHYS2VLSR
;
; PURPOSE:
;       Converts a physical radial velocity (as derived from a
;       Galactic rotation model) into an "observed" V_LSR, as would be
;       reported by a telescope.
;
; CATEGORY:
;       distance-omnibus Rotation Curve Subroutine
;
; CALLING SEQUENCE:
;       vlsr = OMNI_VPHYS2VLSR(l, b, d [,VS=vs][,VOBS=vobs])
;
; INPUTS:
;       L     -- [deg] Galactic Longitude of the line-of-sight
;       B     -- [deg] Galactic Latitude of the line-of-sight
;       D     -- [pc]  Heliocentric Distance (scalar or vector)
;
; OPTIONAL INPUTS:
;       DVDR  -- Deviation from a "flat" rotation curve
;                (Default: MW.DVDR) [km/s/kpc]
;       VS    -- Bulk velocity of HMSFRs in the direction of rotation
;                (negative for a lag or counterrotation)
;                (Default: MW.VS) [km/s]
;       US    -- Bulk velocity of HMSFRs toward the GC
;                (Default: MW.US) [km/s]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       VLSR  -- The LSR velocity as reported by telescopes (kinematic
;                definition of the LSR) for each distance in D.
;
; OPTIONAL OUTPUTS:
;       VOBS  -- VLSR for the "correct" LSR (i.e. the true LSR, not
;                our guesses as to what it is).  Used primarily for
;                debugging purposes.
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  04/27/11, TPEB -- Initial Version.
;       Modified: 05/23/11, TPEB -- Moved calculation of (R,Z) vectors
;                                   to the new routine lbd2rz.pro,
;                                   which takes into account
;                                   inaccuracies in the definition of
;                                   Galactic coordinates.
;       Modified: 06/20/11, TPEB -- Added optional input for the VS
;                                   (correction for HMSFRs).
;       Modified: 06/22/11. TPEB -- Corrected the formula for PHI to
;                                   correctly handle lines of sight in
;                                   Quadrants III and IV.  Also
;                                   corrected sign error in
;                                   calculation of vhelio.
;       Modified: 08/25/11, TPEB -- Code cleanup.
;       Modified: 01/23/12, TPEB -- Combining all VPHYS2VLSR routines
;                                   here, with keyword flags to
;                                   determine values to use in the
;                                   computation.
;       Modified: 02/03/12, TPEB -- Added VOBS optional output for
;                                   spiral structure testing.
;       Modified: 04/02/12, TPEB -- Fixed major-ish bug with
;                                   calculation of non-flat rotation
;                                   curves.  For flat curve, V(r) =
;                                   V0, but that is not true for
;                                   Clemens (1985), for instance.
;                                   Added V(r) variable, and corrected
;                                   the calculation of 'perfect'
;                                   VLSR.
;       Modified: 04/04/12, TPEB -- Fixed minor bug where V0 was a
;                                   1-element array rather than a
;                                   scalar for Clemens.
;       Modified: 07/30/12, TPEB -- Since Clemens (1985) assumes old
;                                   values of V_PEC, need to skip the
;                                   calculation related to the updated
;                                   V_PEC.
;       Modified: 01/29/13, TPEB -- Corrected Clemens (1985) curve
;                                   again, since that curve uses a
;                                   measured +7 km/s in the LSR.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.  Moved ROTC input
;                                   keywords to the galactic_params
;                                   config file.
;       Modified: 09/17/13, TPEB -- Fixed the QIII & QIV thing
;                                   (again).
;       Modified: 11/14/13, TPEB -- Documentation update.
;       Modified: 03/12/14, TPEB -- Really, finally fixed the QIII &
;                                   QIV thingie, because there was an
;                                   issue in OMNI_LBD2RZ where the
;                                   THETA reported was (signed)
;                                   incorrect for QIII & QIV.  The fix
;                                   there and here simplifies the
;                                   matter without the need for
;                                   hijinks or tomfoolery.  Also, get
;                                   fancy with IDL system variables.
;       Modified: 05/08/14, TPEB -- Add check for and definition of
;                                   !CONST system variable.
;       Modified: 09/03/14, TPEB -- Add U_s parameter from Reid et
;                                   al. (2014) into the calculation.
;                                   This produces at most a 0.2-kpc
;                                   correction in the distance to an
;                                   object.
;       Modified: 09/28/14, TPEB -- Add DV/DR parameter from Reid et
;                                   al. (2014) into the calculation.
;                                   Correct convention that V_s is in
;                                   the direction of Galactic
;                                   rotation, and is therefore
;                                   negative for counterrotation;
;                                   change the "-" to "+".
;
;-

FUNCTION OMNI_VPHYS2VLSR, l, b, d, VS=vs, US=us, DVDR=dvdr, VOBS=vobs
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  compat_const                  ; Check for the !CONST system variable
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params config file
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  
  ;; Choose R0 & V0 based on rotation curve desired
  CASE mw.curve OF
     'REID': BEGIN              ; Reid (2014) curve
        R0 = MW.R0
        ;; Create various projected distance arrays
        omni_lbd2rz, l, b, d, r, z, R0=R0, THETA=theta
        V0 = MW.V0
        IF n_elements(dvdr) EQ 0 THEN dvdr = MW.DVDR ; Deviation from flat
        VR = V0 + dvdr * (r - R0)/1.d3               ; DVDR given in km/s/kpc
        IF n_elements(vs) EQ 0 THEN vs = MW.VS       ; Correction for HMSFR (V)
        IF n_elements(us) EQ 0 THEN us = MW.US       ; Ditto (U)
     END
     'IAU': BEGIN               ; IAU Standard Curve
        R0 = 8500.
        ;; Create various projected distance arrays
        omni_lbd2rz, l, b, d, r, z, R0=R0, THETA=theta
        V0 = 220.
        VR = V0
        vs = 0.              
        us = 0.
     END
     'CLEM': BEGIN              ; Clemens (1985) Curve
        R0 = 8500.
        ;; Create various projected distance arrays
        omni_lbd2rz, l, b, d, r, z, R0=R0, THETA=theta
        V0 = (clemens_rotcurve(1.))[0]
        VR = clemens_rotcurve(r / R0)
        vs = 0.
        us = 0.
     END
     'RBAR': BEGIN              ; Reid, with a bar
        R0 = MW.R0  
        ;; Create various projected distance arrays
        omni_lbd2rz, l, b, d, r, z, R0=R0, THETA=theta
        V0 = (reid_bar_rotcurve(1.))[0]
        VR = reid_bar_rotcurve(r / R0)
        IF n_elements(vs) EQ 0 THEN vs = MW.VS   ;; Correction for HMSFR (V)
        IF n_elements(us) EQ 0 THEN us = MW.US   ;; Ditto (U)
     END
     'SPTST': BEGIN             ; Junk?
        R0 = MW.R0
        omni_lbd2rz, l, b, d, r, z, R0=R0, THETA=theta
        V0 = 250.
        VR = V0
        IF n_elements(vs) EQ 0 THEN vs = MW.VS   ;; Correction for HMSFR (V)
        IF n_elements(us) EQ 0 THEN us = MW.US   ;; Ditto (U)
     END
     ELSE: message,'Unrecognized rotation curve '+mw.curve+$
                   ' specified in galactic-params'
  ENDCASE
  
  ;; Angle between LOS and Velocity Vector
  phi = !dpi/2.d - l*!const.dtor - theta
  
  ;; This is VLSR for correct LSR
  vobs = (vR+vs)*cos(b*!const.dtor)*cos(phi) - v0*sin(l*!const.dtor) + $
         (us)*cos(b*!const.dtor)*sin(phi) ;; Correction for U_s motion
  ;; The IAU curve assumes that observed LSR is the 'correct' LSR
  IF mw.curve EQ 'IAU' THEN RETURN,vobs
  
  ;; Heliocentric velocity, using Solar Pecular Motion from
  ;;   galactic-params confiuration file
  vhelio = vobs - (mw.bigu*cos(l*!const.dtor)+mw.bigv*sin(l*!const.dtor)) * $
           cos(b*!const.dtor) - mw.bigw*sin(b*!const.dtor)
  
  ;; For Clemens (1985) curve, they also found a +7(1.5) km/s motion
  ;; of the LSR in the BIGV direction.  So, add this to vobs:
  IF mw.curve EQ 'CLEM' THEN RETURN,vobs+(7.d * sin(l*!const.dtor) * $
                                          cos(b*!const.dtor))
  
  ;; OLD (i.e. STANDARD) DEFINITION OF V_PEC:
  ;;
  ;; From Jackson, et al., 2006 (GRS Paper) IAU Kinematic LSR:
  ;; More detailed numbers:
  ;; http://www.gb.nrao.edu/~fghigo/gbtdoc/doppler.html
  ;; Gordon (1975)  "Computer Programs for Radio Astronomy," by
  ;; M.A.Gordon, page 281, in "Methods of Experimental Physics:
  ;; Volume 12: Astrophysics, Part C: Radio Observations",
  ;; ed. M.L.Meeks, Academic Press 1976.  
  solarmotion_ra  = ((18+03/6d1+50.29/3.6d3)*15)
  solarmotion_dec = (30+00/6d1+16.8/3.6d3) 
  solarmotion_mag = 20.d
  
  ;; Get RA, Dec of Galactic line-of-sight
  euler, l, b, ra, dec, 2
  gcirc, 2, solarmotion_ra, solarmotion_dec, ra, dec, theta2
  
  ;; This is the VLSR that a telescope would report (VLSR for each of
  ;; the distances in the dvec array)
  RETURN, vhelio+solarmotion_mag*cos(theta2/206265.)
  
END
