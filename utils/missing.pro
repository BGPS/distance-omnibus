;+
; NAME:
;    missing
; PURPOSE:
;    Gives the indicies of those elements of vector B which
;    are not present in vector A.  Basically WHERE(B NOT IN A)
;    where B and A are 1 dimensional arrays.  A and B must
;    not have repeating elements.
; CATEGORY:
;    array
; CALLING SEQUENCE:
;    indicies = MISSING(a,b,[count])
; INPUTS:
;    A       vector that might contains elements of vector B
;    B       vector that we would like to know which of its
;              elements do not exist in A
; OUTPUTS:
;    Index into B of elements not found in vector A.  If all of
;    the matches are found, then -1 is returned.  If the function is called
;    with incorrect arguments, a warning is displayed, and -2 is
;    returned (see SIDE EFFECTS for more info)
;
; OPTIONAL OUTPUTS:
;    count           The number of non-matching elements
; SEE ALSO:
;    where_array
; NOTES:
;    where_array using N_ELEMENTS(A) x N_ELEMENTS(B) auxillary
;    storage, so don't do this for huge arrays.  This hasn't been
;    added to where_array because it doesn't handle repeated elements
;    correctly.
; MODIFICATION HISTORY:
;    This is based on WHERE_ARRAY.
;-
FUNCTION missing, A, B, count

COMPILE_OPT IDL2, STRICTARRSUBS

count = 0

; Check for: correct number of parameters
;                        that A and B have each only 1 dimension
;                        that A and B are defined
IF N_PARAMS() GT 3 OR (SIZE(A))[0] NE 1 OR (SIZE(B))[0] NE 1 $
  OR n_elements(A) EQ 0 OR N_ELEMENTS(B) EQ 0 THEN BEGIN
   MESSAGE,'Inproper parameters',/CONTINUE
   MESSAGE,'Usage: result = missing(A,B,[count]',/CONTINUE
   RETURN,-2
ENDIF

nB = N_ELEMENTS(B)

wpresent = WHERE_ARRAY( A, B, count_present )
count = nB - count_present
IF count_present EQ nB THEN RETURN,-1
IF count_present EQ 0 THEN RETURN,INDGEN(nB)

present = BYTARR( nB )
present[wpresent] = 1b

RETURN,WHERE( ~ present ,count)

END
