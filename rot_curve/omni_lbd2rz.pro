;+
; NAME:
;       OMNI_LBD2RZ
;
; PURPOSE:
;       Convert a triad of (l,b,d) into Galactocentric cylindrical
;       coordinates (R,Z) taking into account the inaccurate nature of
;       the Galactic Coordinate syatem defined by Blaauw et al. (1960,
;       MNRAS, 121, 123) -- namely the vertical Solar offset from the
;       Galactic midplane.
;
; CATEGORY:
;       distance-omnibus Coordinate Transformation
;
; CALLING SEQUENCE:
;       OMNI_LBD2RZ, l, b, d, R, Z
;
; INPUTS:
;       L  -- Galactic Longitude    (degrees)
;       B  -- Galactic Latitude     (degrees)
;       D  -- Heliocentric Distance (pc)
;
; OPTIONAL INPUTS:
;       R0 -- Radius of the Solar Circle [Default: !MW.R0]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       R  -- Galactocentric Radius          (pc)
;       Z  -- Height above Galactic Midplane (pc)
;
; OPTIONAL OUTPUTS:
;       DPROJ -- The heliocentric distance projected onto the Galactic Plane
;       THETA -- The angle between GC line and R vector to object
;       XGAL  -- Galactic X-position (Rgal * 
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; NOTES:
;
; Calculates the Galactocentric coordinates by converting (l,b,d) into
; local Catesian coordinates.  First, we perform a rotation (180 deg) to
; put Theta=0 along LOS to GC, but away from GC.  Next, we translate
; the center of the coordinates to the GC.  Finally, we rotate the
; coordinates to remove the vertical offset (Z0) of the Sun w.r.t. to
; the Galactic midplane.  To obtain (R,Z), we do the usual conversion
; from cartesian -> cylindrical coordinates.
;
; (x)   (Cos[th]  0 Sin[th]) (1 0 0 -R0) (-1 0 0 0) (-d Cos[b] Cos[l])
; |y| = |  0      1   0    |.|0 1 0  0 |.|0 -1 0 0|.|-d Cos[b] Sin[l]|
; |z|   |-Sin[th] 0 Cos[th]| |0 0 0  0 | |0  0 1 0| |    d Sin[b]    |
; (1)   (  0      0   1    ) (0 0 0  1 ) (0  0 0 1) (       1        )
;
; Where th = (Z0 / R0) is the angle the LOS from the Sun to the GC
; makes w.r.t. the Galactic Midplane.
;
; The correction for the location of Sgr A* is small (~260"),
; and I don't feel like including the proper rotation matrices
; at this time (effects are at the 1 part in 10^5 level anyways).
; For the record, Reid & Brunthaler (2004, ApJ, 616, 872) locate Sgr
; A* at (l,b) = (359.9443, -0.0462).
;
; MODIFICATION HISTORY:
;
;       Created:  05/23/11, TPEB -- Inital Version.
;       Modified: 05/23/11, TPEB -- Removed original scheme for
;                                   correcting for the location of Sgr
;                                   A*.
;       Modified: 08/16/11, TPEB -- Added optional output DPROJ for
;                                   the projected heliocentric
;                                   distance, and THETA for the angle
;                                   between GC line and R vector to
;                                   object.
;       Modified: 08/25/11, TPEB -- Added optional input of R0 for use
;                                   with different rotation curves,
;                                   and fixed issue where a complex
;                                   distance input was converted to
;                                   real.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.
;       Modified: 09/05/13, TPEB -- Added XGAL and YGAL coordinate
;                                   optional outputs.
;
;-

PRO OMNI_LBD2RZ, l, b, d, R, Z, R0=R0, DPROJ=dproj, THETA=theta, $
                 XGAL=xgal, YGAL=ygal
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params & dpdf-params config files
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  
  ;; Parse input -- Radius of the Solar Circle (pc)
  IF n_elements(R0) EQ 0 THEN  R0 = mw.r0
  
  ;; Simplify variables -- and compute angle TH of sun above midplane
  l2 = double(l)*(!dpi/180.d)   ; (Un-)Corrected GLON (rad)
  b2 = double(b)*(!dpi/180.d)   ; (Un-)Corrected GLAT (rad)
  th = asin(double(mw.z0) / R0) ; Angle of LOS to GC above midplane (rad)
  
  ;; Check on the type of d -- make double version of real or complex
  IF size(d,/type) LE 5 THEN d = double(d) ELSE d = dcomplex(d)
  
  ;; Simplify inputs
  x1 = -d * cos(b2) * cos(l2)
  y1 = -d * cos(b2) * sin(l2)
  z1 =  d * sin(b2)
  
  ;; Calculate R & Z
  R = sqrt( y1*y1 + ((R0+x1)*cos(th) - z1*sin(th))^2)
  Z = z1*cos(th) + (R0+x1)*sin(th)
  
  ;;==========================================================
  ;; Optional outputs
  
  ;; Heliocentric distance projected into the Galactic Plane
  dproj = sqrt( y1*y1 + (x1*cos(th) - z1*sin(th))^2 )
  
  ;; Angle between GC line and R vector to object
  theta = acos((R0*cos(th)-dproj*cos(l2))/R)
  
  ;; Galactocentric X & Y
  xgal = -R * cos(theta)
  ygal = -R * sin(theta)
  
  RETURN
END
