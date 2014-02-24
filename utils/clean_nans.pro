;+
; NAME:
;       CLEAN_NANS
;
; PURPOSE:
;       Replace NaN values in an array with zeros or other useful value.
;
; CATEGORY:
;       Utility Routine
;
; CALLING SEQUENCE:
;       nclean = CLEAN_NANS( array [,REPLACE=replace])
;
; INPUTS:
;       ARRAY   -- Input array for cleansing
;
; OPTIONAL INPUTS:
;       REPLACE -- Replacement value for NaN's [default = 0.0d]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NCLEAN  -- Number of NaN values replaced
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;       Written by Timothy Ellworth Bowers, 2/8/10
;         Based on algorithm from Alex Conley
;       Modified: 06/27/11, TPEB -- Incorportated into
;                                   distance-omnibus repository.
;-

FUNCTION CLEAN_NANS, array, REPLACE=replace
  
  IF n_elements(replace) EQ 0 THEN replace = 0.d
  
  w = WHERE( ~ FINITE( array ), nclean )
  IF nclean NE 0 THEN array[w] = double(replace)
  
  RETURN, nclean
  
END
