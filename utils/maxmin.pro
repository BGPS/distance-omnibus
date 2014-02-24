;+
; NAME:
;       MAXMIN
;
; PURPOSE:
;       Returns the [max,min] of an array.  Useful for dealing with
;       ranges in longitude / RA plots.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       result = MAXMIN(array)
;
; INPUTS:
;       ARRAY -- An IDL numeric scalar, vector or array.
;
; OPTIONAL INPUTS:
;      DIMEN -- integer (either 1 or 2) specifying which dimension of
;               a 2-d array to  take the minimum and maximum.   Note
;               that (unlike the DIMENSION keyword to the MIN()
;               function) DIMEN is only valid for a 2-d array, larger
;               dimensions are  not supported. 
;
; KEYWORD PARAMETERS:
;       NAN -- Set this keyword to cause the routine to check for
;              occurrences of the IEEE floating-point values NaN or
;              Infinity in the input data. Elements with the value NaN
;              or Infinity are treated as missing data.
;
; OUTPUTS:
;      RESULT -- A two element vector (if DIMEN is not supplied)
;                result[0] = maximum value of array
;                result[1] = minimum value of array
;
;                If the DIMEN keyword is supplied then value will be a
;                2 x N element array where N is the number of elements
;                in the specified dimension
;
; OPTIONAL OUTPUTS:
;       SUBS -- Two-dimensional vector; the first element gives the
;               subscript of the minimum value, the second element
;               gives the subscript of the maximum value.     
;
; MODIFICATION HISTORY:
;
;       Created:  10/24/12, TPEB -- Initial version.  Simple wrapper
;                                   for minmax.pro
;
;-

FUNCTION MAXMIN, array, subs, NAN=nan, DIMEN=dimen
  
  mm = minmax(array,subs,NAN=nan,DIMEN=dimen)
  
  IF n_elements(mm) GT 2 THEN BEGIN
     
     mm2 = mm
     mm2[0,*] = mm[1,*]
     mm2[1,*] = mm[0,*]
     
     RETURN,mm2
     
  ENDIF ELSE RETURN, REVERSE(mm)
  
END
