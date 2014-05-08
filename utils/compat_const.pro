;+
; NAME:
;       COMPAT_CONST
;
; PURPOSE:
;       Compatibility routine to define the !CONST system variable if
;       it does not already exist (!CONST was introduced with IDL
;       v8.2).
;
;       From the IDL description of the !CONST system variable
;       (http://www.exelisvis.com/docs/constant_system_variable.html):
;
;       A read-only structure variable that contains physical
;       constants.  All values are in double precision and are
;       given in Meter-Kilogram-Second (MKS) units. The values
;       for fundamental physical constants are taken from the
;       Committee on Data for Science and Technology, "2010
;       CODATA Recommended Values," available from the Physical
;       Measurement Laboratory at the National Institute of
;       Standards and Technology (NIST). The values for au, G,
;       M_earth, M_sun, parsec, and R_earth are derived from
;       the IAU 2009 System of Astronomical Constants, based
;       upon the Barycentric Coordinate Time (TCB) values. The
;       light-year distance was computed using the speed of
;       light during 1 Julian year of 365.25 days*86400
;       seconds. The value for π (pi) was computed using
;       Buffon's needle method with approximately 253 tosses of
;       4 cm toothpicks onto parallel lines. 
;
; CATEGORY:
;       Compatibility Utility
;
; CALLING SEQUENCE:
;       COMPAT_CONST
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE (Defines system variable !CONST if not already present.)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; NOTES:
;       ***** This routine creates the system variable as extant in
;       IDL v8.2.3!  This is the version of IDL used to develop
;       distance-omnibus, but if future changes to !CONST conflict
;       with the version here, someone will need to fix it. *****
;
; MODIFICATION HISTORY:
;
;       Created:  05/08/14, TPEB -- Initial version.
;
;-

PRO COMPAT_CONST
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Check for the existance of the !CONST system variable.
  defsysv, '!CONST', EXISTS=exists
  IF exists THEN RETURN         ; Return if it already exists
  
  defsysv,'!CONST',{ALPHA:0.0072973525698d,$,$
                    AU:149597870700.d,$
                    C:299792458.d,$
                    DTOR:0.0174532925199433d,$
                    E:1.602176565d-19,$
                    EPS0:8.854187817d-12,$
                    EULER:2.7182818284590451d,$
                    F:96485.3365d,$
                    G:6.674284d-11,$
                    GN:9.80665d,$
                    H:6.62606957d-34,$
                    HBAR:1.054571726d-34,$
                    I:DCOMPLEX(0.,1.),$
                    K:1.3806488d-23,$
                    LY:9460730472580800.d,$
                    M_EARTH:5.97218639d+24,$
                    M_SUN:1.98841586057d+30,$
                    ME:9.10938291d-31,$
                    MN:1.674927351d-27,$
                    MP:1.672621777d-27,$
                    MU0:1.2566370614d-06,$
                    N0:2.6867805d+25,$
                    NA:6.02214129d+23,$
                    P0:101325.d,$
                    PARSEC:30856775814671912.d,$
                    PHI:1.6180339887498949d,$
                    PI:3.1415926535897931d,$
                    R:8.3144621d,$
                    R_EARTH:6378136.6d,$
                    RTOD:57.295779513082323d,$
                    RE:2.8179403267d-15,$
                    RYDBERG:10973731.568539d,$
                    SIGMA:5.670373d-08,$
                    T0:273.15d,$
                    U:1.660538921d-27,$
                    VM:0.022413968d}
  RETURN
END

