;+
; NAME:
;       OMNI_PHYSICAL_CONSTANTS
;
; PURPOSE:
;       Load physical constants into a SYSTEM VARIABLE instead of
;       always hunting around for the values, etc.
;
; CATEGORY:
;       disstance-omnibus Utility
;
; CALLING SEQUENCE:
;       OMNI_PHYSICAL_CONSTANTS
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
;       NONE (Loads constants into the !PC SYSTEM VARIABLE structure.)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  12/02/13, TPEB -- Initial version.
;
;-

PRO OMNI_PHYSICAL_CONSTANTS
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  defsysv, '!PC', $
           {c     : 299792458.d,$     ; Speed of Light     [m/s]
            kb    : 1.3806503d-23,$   ; Boltzmann constant [m^2 kg s^-2 K^-1]
            h     : 6.626068d-34,$    ; Planck constant    [m^2 kg s^-1]
            Jy    : 1.d-26,$          ; Jansky             [W m^-2 Hz^-1]
            kpc   : 3.08567758d19,$   ; kiloparsec         [m]
            msun  : 1.9891d30,$       ; Solar Mass         [kg]
            pc    : 3.08567758d16,$   ; parsec             [m]
            G     : 6.67384d-11,$     ; Big G              [m^3 kg^-1 s^-2]
            mu    : 2.8d,$            ; Avg molecular mass [a.m.u.]
            mh    : 1.673723599d-27}  ; Hydrogen Mass      [kg]
  
  RETURN
END
