;+
; NAME:
;       MODE
;
; PURPOSE:
;       Finds the mode (value with the maximum frequency distribution)
;       of an array of INTEGER values.
;
; CATEGORY:
;       Statistics
;
; CALLING SEQUENCE:
;       result = MODE( array )
;
; INPUTS:
;       ARRAY  -- The array of INTEGER values for which to find the mode
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RESULT -- The mode of the array.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  04/28/11, TPEB -- Codified version of a routine
;                                   downloaded from somewhere on the
;                                   internets.
;
;-

FUNCTION MODE, array

  ON_ERROR, 2
  
  ;; Check input
  IF n_elements(array) EQ 0 THEN message,'Must pass an array argument.'
  
  ;; Check that array is if type INT
  dataType = size(array, /Type)
  IF ((dataType GT 3) AND (dataType LT 12)) THEN $
     message,'Data is not INTEGER type.'
  
  ;; Calculate the distribution frequency
  distfreq = histogram(array, MIN=Min(array))
  
  ;; Find the maximum of the frequency distribution.
  maxfreq = Max(distfreq)
  
  ;; Find the mode.
  mode = Where(distfreq EQ maxfreq, count) + Min(array)
  
  ;; Warn the user if the mode is not singular.
  IF count NE 1 THEN message,'The MODE is not singular.',/inf
  
  RETURN, mode
  
END
