;+
; NAME:
;       CLEMENS_ROTCURVE
;
; PURPOSE:
;       Calculates the Galactic rotation curve presented in Clemens
;       (1985, ApJ, 295, 422).
;
; CATEGORY:
;       Novelty
;
; CALLING SEQUENCE:
;       theta = CLEMENS_ROTCURVE( R )
;
; INPUTS:
;       R    -- Galactocentric radius, normalized to R0
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       THETA -- Rotational velocity at the radius R
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  06/21/11, TPEB -- Initial Version
;      
;-

FUNCTION CLEMENS_ROTCURVE, R
  
  
  A = [0., 3069.81, -15809.8, +43980.1, -68287.3, +54904., -17731.]
  B = [+325.0912, -248.1467, +231.87099, -110.73531, +25.073006, -2.110625]
  C = [-2342.6564, +2507.60391, -1024.068760, +224.562732, -28.4080026, $
      +2.0697271, -0.08050808, +0.00129348]
  D = 234.88
  
  ma = R LT 0.09
  mb = R GE 0.09 AND R LT 0.45
  mc = R GE 0.45 AND R LT 1.6
  md = R GE 1.6
  
  ;; Loop over A's
  va = fltarr(n_elements(r))
  FOR i=0L, 6 Do va += A[i]*(R*8.5)^i
  va *= ma
  
  ;; Loop over B's
  vb = fltarr(n_elements(r))
  FOR i=0L, 5 DO vb += B[i]*(R*8.5)^i
  vb *= mb
  
  ;; Loop over C's
  vc = fltarr(n_elements(r))
  FOR i=0L, 7 DO vc += C[i]*(R*8.5)^i
  vc *= mc
  ;;print,m4_stat(vc)

  ;; Loop over A's
  vd = D * md
  
  vel = va + vb + vc + vd
  
  
  ;; plot,r,vel,yr=[0,275],/yst,/xst,xtit='R!dgal!n / R!d0!n',$
  ;;      ytit='Rotation Velocity [km s!u-1!n]',/nodata
  ;; oplot,r,vel,thick=2,color=cgColor('Red')
  ;; oplot,r,vel*0.+254.,thick=2,color=cgColor('Forest Green')
  ;; oplot,r,vel*0.+220.,thick=2,color=cgColor('Dodger Blue')
  ;; vline,1.
  
  RETURN,vel
END
