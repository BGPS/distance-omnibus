;+
; NAME:
;       RMS_DG_SPEC
;
; PURPOSE:
;       Calculate the r.m.s. of a baselined dense gas spectrum [HHT:
;       HCO+/N2H+. CSO: CS(5-4), ARO12m: CS(2-1)]
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       rms = RMS_HHT_SPEC(v, spec)
;
; INPUTS:
;       V    -- Velocity scale of the spectrum
;       SPEC -- Baselined spectrum
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RMS  -- The r.m.s. noise of the baselined HHT spectrum.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  11/01/11, TPEB -- Initial Version.
;
;-

FUNCTION RMS_DG_SPEC, v, sp
  
  ;; Velocity ranges for r.m.s. [km/s]
  v_lo = [-150,-110]
  v_hi = [210,250]
  
  rind = WHERE( (v GE v_lo[0] AND v LE v_lo[1]) OR $
                (v GE v_hi[0] AND v LE v_hi[1]), n)
  
  IF n EQ 0 THEN BEGIN
     message,'No points in the specified velocity ranges.',/cont
     return,!values.f_nan
  ENDIF
  
  RETURN, stddev(sp[rind])
END
