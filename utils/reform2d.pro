;+
; NAME:
;       REFORM2D
;
; PURPOSE:
;       Take a 2d slice from a 3d or 4d array and make it a 2d array.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       array = REFORM2D( slice )
;
; INPUTS:
;       SLICE -- 2D slice from a 3d or 4d array
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       ARRAY -- 2D array
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  09/14/12, TPEB -- Initial version.
;
;-

FUNCTION REFORM2D, slice
  
  dims = size(slice,/DIM)
  ind = WHERE(dims NE 1, n1)
  
  IF n1 NE 2 THEN BEGIN
     message,'Not a 2d array.  Returning input.'/cont
     RETURN,slice
  ENDIF
  
  array = reform(slice, dims[ind[0]], dims[ind[1]])
  
  RETURN,array
END
