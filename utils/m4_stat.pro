;+
; NAME:
;       M4_STAT
;
; PURPOSE:
;       Compiles the 4 M's statistics: Mean, Median, Min, Max
;       for an input array.
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       result = M4_STAT( array1, array2, ..., array10 )
;
; INPUTS:
;       ARRAY1   -- The first array for which M4 statistics are desired.
;       ARRAY2   -- The second array for which M4 statistics are desired.
;         ...
;       ARRAY10  -- The tenth array for which M4 statistics are desired.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RESULT -- An array of 4-element arrays containing: 
;                 [Mean, Median, Min, Max] of the input array(s).
;
; OPTIONAL OUTPUTS:
;       CT -- The number of valid arrays in the returned matrix.
;
; MODIFICATION HISTORY:
;
;       Created:  04/19/11, TPEB -- Initial version.
;       Modified: 10/17/12, TPEB -- Added /EVEN command to MEDIAN.
;       Modified: 02/04/14, TPEB -- Allow for multiple arrays (up to 10)
;
;-

FUNCTION M4_STAT, array1, array2, array3, array4, array5, array6, array7, $
                  array8, array9, array10, CT=ct
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  ON_ERROR,2             ;; Return to caller
  
  output = !null
  ct = 0b
  
  IF n_elements (array1) NE 0 THEN BEGIN
     IF n_elements(array1) EQ 1 THEN array1 = [array1]
     
     output = [[output],[MEAN(array1, /NAN), MEDIAN(array1,/EVEN), $
                         MIN(array1, /NAN), MAX(array1, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array2) NE 0 THEN BEGIN
     IF n_elements(array2) EQ 1 THEN array2 = [array2]
     
     output = [[output],[MEAN(array2, /NAN), MEDIAN(array2,/EVEN), $
                         MIN(array2, /NAN), MAX(array2, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array3) NE 0 THEN BEGIN
     IF n_elements(array3) EQ 1 THEN array3 = [array3]
     
     output = [[output],[MEAN(array3, /NAN), MEDIAN(array3,/EVEN), $
                         MIN(array3, /NAN), MAX(array3, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array4) NE 0 THEN BEGIN
     IF n_elements(array4) EQ 1 THEN array4 = [array4]
     
     output = [[output],[MEAN(array4, /NAN), MEDIAN(array4,/EVEN), $
                         MIN(array4, /NAN), MAX(array4, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array5) NE 0 THEN BEGIN
     IF n_elements(array5) EQ 1 THEN array5 = [array5]
     
     output = [[output],[MEAN(array5, /NAN), MEDIAN(array5,/EVEN), $
                         MIN(array5, /NAN), MAX(array5, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array6) NE 0 THEN BEGIN
     IF n_elements(array6) EQ 1 THEN array6 = [array6]
     
     output = [[output],[MEAN(array6, /NAN), MEDIAN(array6,/EVEN), $
                         MIN(array6, /NAN), MAX(array6, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array7) NE 0 THEN BEGIN
     IF n_elements(array7) EQ 1 THEN array7 = [array7]
     
     output = [[output],[MEAN(array7, /NAN), MEDIAN(array7,/EVEN), $
                         MIN(array7, /NAN), MAX(array7, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array8) NE 0 THEN BEGIN
     IF n_elements(array8) EQ 1 THEN array8 = [array8]
     
     output = [[output],[MEAN(array8, /NAN), MEDIAN(array8,/EVEN), $
                         MIN(array8, /NAN), MAX(array8, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array9) NE 0 THEN BEGIN
     IF n_elements(array9) EQ 1 THEN array9 = [array9]
     
     output = [[output],[MEAN(array9, /NAN), MEDIAN(array9,/EVEN), $
                         MIN(array9, /NAN), MAX(array9, /NAN)]]
     ct++
  ENDIF
  
  IF n_elements (array10) NE 0 THEN BEGIN
     IF n_elements(array10) EQ 1 THEN array10 = [array10]
     
     output = [[output],[MEAN(array10, /NAN), MEDIAN(array10,/EVEN), $
                         MIN(array10, /NAN), MAX(array10, /NAN)]]
     ct++
  ENDIF
  
  RETURN,output
  
END
