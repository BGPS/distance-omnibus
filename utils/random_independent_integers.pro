;+
; NAME:
;       RANDOM_INDEPENDENT_INTEGERS
;
; PURPOSE:
;       Generate a set of independent random numbers, as if drawing
;       numbers from a bowl but not returning previously-drawn numbers.
;
; CATEGORY:
;       Random Number Generation Utility
;
; CALLING SEQUENCE:
;       array = random_independent_integers (n, max [,MIN=min])
;
; INPUTS:
;       n    -- Number of independent integers to generate
;       max  -- Largest possible integer to generate
;
; OPTIONAL INPUTS:
;       min  -- Smallest possible integer to generate.  [Default: 1]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       array  -- Array of n independent random integers
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  06/01/09, TPEB -- Initial Version
;       Modified: 10/05/10, TPEB -- Added MIN keyword
;       Modified: 07/22/13, TPEB -- Overhauled to use the PERMUTE
;                                   routine from IDLASTRO.
;       
;-

FUNCTION RANDOM_INDEPENDENT_INTEGERS, n, max, MIN=min
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  ON_ERROR, 2
  
  ;; Parse keywords
  min = KEYWORD_SET(min) ? long(min) : 1L
  max = long(max)
  range = max - min + 1
  n   = long(n) < range         ; Don't let n be larger than range
  
  ;; Generate array of numbers from min to max
  arr = lindgen(range) + min
  
  ;; Use PERMUTE to randomize the indices of arr
  ind = permute(range)
  
  ;; Return the first n elements
  RETURN,arr[ind[0:n-1]]
  
END
