;+
; NAME:                
;       EXIST
;
; PURPOSE:              
;       Check whether a variable exists or not using the size
;       function. 
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       res = EXIST( var )
;
; INPUTS:
;       VAR -- Variable whose existance you wonder about.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RES -- Boolean T/F
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  07/19/10, TPEB -- Initial (undocumented) version from
;                                   some dark, mysterious past (I stole
;                                   the code).
;       Modified: 07/14/11, TPEB -- Documentation!
;       Modified: 02/28/13, TPEB -- Greatly simplified this already
;                                   simple routine.
;       Modified: 03/21/13, TPEB -- Added COMPILE_OPT statement.
;       Modified: 08/21/13, TPEB -- Further simplified this already
;                                   simplified routine.
;
;-

FUNCTION EXIST, var
  
  COMPILE_OPT IDL2, HIDDEN
  ON_ERROR,2                    ; Return to caller if an error occurs
  
  RETURN, (size(var))[1] NE 0
END
