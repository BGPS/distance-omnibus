;+
; NAME:
;       CLOSEST_ELLIPSE
;
; PURPOSE:
;       Determines the closest ellipse to a position by considering
;       shape and size of ellipse, not just distance to centroid.
;
; CATEGORY:
;       Coordinate utility
;
; CALLING SEQUENCE:
;       ind = CLOSEST_ELLIPSE( A1, B1, A2, B2, MAJ, MIN, PA
;             [POINT_ELLIPSE=dist] [,/SIG_DEG] )
;
; INPUTS:
;       A1      -- Coordinate A (RA,  l) of object in question
;       B1      -- Coordinate B (Dec, b) of object in question
;       A2      -- Vector of corrdinates A (RA,  l) of comparison set
;                  ellipse centroids
;       B2      -- Vector of corrdinates B (dec, b) of comparison set
;                  ellipse centroids
;       MAJ     -- Vector of comparison ellipse semi-major axes (in arcsec)
;       MIN     -- Vector of comparison ellipse semi-minor axes (in arcsec)
;       PA      -- Vector of comparison ellipse position angles
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       SIG_DEG -- Comparison ellipse major / minor axes are in
;                  degrees (rather than assumed arcsec).
;
; OUTPUTS:
;       IND     -- Index of ellipse from comparison set which lies
;                  closest to the object position.
;
; OPTIONAL OUTPUTS:
;       POINT_ELLIPSE -- Vector of distances (along the +r(hat)
;                        direction) from the ellipse-crossing point to
;                        the position in question.  The returned IND
;                        is the minimum element of this vector.
;
; EXAMPLE:
;       ind = closest_ellipse(53.30,0.04,pf_irdc.l,pf_irdc.b,pf_irdc.maj,
;                             pf_irdc.min,pf_irdc.pa_irdc, POINT_ELLIPSE=dist)
;
; NOTES:
;       Written following the algorithm at
;       http://my.opera.com/Vorlath/blog/show.dml/476448 "Algorithms:
;       Distance From Point to Ellipse, An Easier Method", with
;       generalities to deal with ellipses of random position angle.
;
; MODIFICATION HISTORY:
;
;       Created:  02/14/11, TPEB -- Initial version, following
;                                   algorithm mentioned above.
;       Modified: 02/15/11, TPEB -- Corrected covariance elements, and
;                                   added POINT_ELLIPSE optional output.
;       Modified: 02/15/11, TPEB -- Caught bug: brought into
;                                   compliance with semi-major axes.
;-

FUNCTION CLOSEST_ELLIPSE, A1, B1, A2, B2, MAJ, MIN, PA, POINT_ELLIPSE=pt_el, $
                          SIG_DEG=sig_deg
  
  IF n_elements(sig_deg) NE 0 THEN conv = 1.d ELSE conv = 3600.d
  
  ;; Conversions
  theta = PA * !dtor
  sig_min = MIN / conv
  sig_maj = MAJ / conv
  
  ;; Calculate general ellipse parameters (covariance elements)
  alpha = (cos(theta))^2 / (sig_min^2) + $
          (sin(theta))^2 / (sig_maj^2)
  beta  = -sin(2.*theta) / (2.d * sig_min^2) + $
          sin(2.*theta) / (2.d * sig_maj^2)
  gamma = (sin(theta))^2 / (sig_min^2) + $
          (cos(theta))^2 / (sig_maj^2)
  
  
  ;; Calculate point of intersection
  l = A2 + (A1-A2) / $
      sqrt( alpha*(A1-A2)^2 + 2*beta*(A1-A2)*(B1-B2) + gamma*(B1-B2)^2 )
  b = B2 + (B1-B2) / $
      sqrt( alpha*(A1-A2)^2 + 2*beta*(A1-A2)*(B1-B2) + gamma*(B1-B2)^2 )
  
  ;; Calculate distances
  pt_cen = sqrt( (A1 - A2)^2 + (B1 - B2)^2 )   ;; Point to centroid
  el_cen = sqrt( (l - A2)^2  + (b - B2)^2  )   ;; Ellipse crossing to cent.
  pt_el = pt_cen - el_cen                      ;; Point to ellipse crossing
                                               ;; NOTE: This is a signed qty!
  md = MIN( pt_el, ind )
  
  RETURN,ind
END
