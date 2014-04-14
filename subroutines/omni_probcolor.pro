;+
; NAME:
;       OMNI_PROBCOLOR
;
; PURPOSE:
;       Generate the proper colors for the probability plots using the
;       IDL-Coyote routine cgColor().
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       color = OMNI_PROBCOLOR( index [,/NAME] )
;
; INPUTS:
;       INDEX  -- Probability method index # to look up (may be scalar
;                 or vector).
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NAME   -- Return the string color name (from cgColor) instead
;                 of the color number (needed as input for some
;                 routines such as al_legend).
;
; OUTPUTS:
;       COLOR -- Color needed by the plotting routines
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  XX/XX/XX, TPEB -- Initial Version
;       Modified: 10/17/10, TPEB -- Added functionality for arrays of
;                                   index, and implimented CASE
;                                   structure.
;       Modified: 09/12/11, TPEB -- Made routine conform with the
;                                   cgColor utility from the
;                                   IDL-Coyote library.
;       Modified: 09/15/11, TPEB -- Added color for PARALLAX association,
;                                   and corrected the ELSE statement
;                                   to do what it says it does.
;       Modified: 03/06/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.  Removed direct
;                                   association between color and DPDF
;                                   type.
;       Modified: 03/07/13, TPEB -- Changed color #1 to 'BLU5' to
;                                   conform with EMAF paper plot.
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;
;-

FUNCTION OMNI_PROBCOLOR, index, NAME=name
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Check length of input i -- create output, and begin loop
  index = long(index)
  ncol = n_elements(index)
  color = strarr(ncol)
  
  FOR jj = 0L, ncol-1 DO BEGIN
     CASE index[jj] OF
        0: color[jj] = 'GRN6'                 ;; Usually KDIST
        1: color[jj] = 'BLU5'                 ;; Usually H2
        2: color[jj] = 'Crimson'              ;; Usually EMAF
        3: color[jj] = 'BLU7'                 ;; 
        4: color[jj] = 'Goldenrod'            ;; 
        5: color[jj] = 'PUR6'                 ;; 
        6: color[jj] = 'BLK6'                 ;; 
        -100: color[jj] = 'Opposite'          ;; Total Probability
        ELSE: BEGIN
           message,'Color index '+string(index[jj],FORMAT="(I0)")+$
                   ' not implemented!  Returning OPPOSITE...',/cont
           color[jj] = 'Opposite'
        ENDCASE
     ENDCASE
  ENDFOR
  
  ;; Return name or color values
  IF KEYWORD_SET(name) THEN RETURN,color ELSE RETURN,cgColor(color)
  
END
