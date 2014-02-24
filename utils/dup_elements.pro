;+
; NAME:
;       DUP_ELEMENTS
;
; PURPOSE:
;       Finds duplicate elements in an (integer) array, and returns
;       the values of the duplicates.  Uses the HISTOGRAM function.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       result = DUP_ELEMENTS( array, COUNT=count )
;
; INPUTS:
;       array  -- input array to search
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       result -- list of duplicate values (returns scalar zero if no
;                 duplicates found)
;
; OPTIONAL OUTPUTS:
;       count --- number of duplicated items in array
;
; MODIFICATION HISTORY:
;
;       Created:  10/07/10, TPEB -- Initial Version
;-

FUNCTION DUP_ELEMENTS, array, COUNT=count, REPEATS=repeats
 
  ;; Force array to be of type long for this to work...
  array = long(array)
  
  hist = HISTOGRAM(array, binsize=1, /NAN, /L64, loc=vals)
  
  ind = WHERE( hist GT 1., count)
    
  repeats = hist[ind]
  
  RETURN,vals[ind]
END
