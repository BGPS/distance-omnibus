;+
; NAME:
;       EXTRACT_SUBSECTION
;
; PURPOSE:
;       Extract a subsection of an image (2-D array), being careful of
;       image edges (error checking)
;
; CATEGORY:
;       Image manipulation
;
; CALLING SEQUENCE:
;       result = extract_subsection(array_in, start, subsize)
;
; INPUTS:
;       ARRAY_IN  -- Input full 2-D array (image)
;       START     -- Starting pixel (2-element array)
;       SUBSIZE   -- Size of the subsection to be returned (2-element array)
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RESULT    -- Extracted subsection
;
; OPTIONAL OUTPUTS:
;       ELEMENTS  -- 2x2 array of actually-used start & end points of
;                    the subsection
;       FLAG      -- Set to 1b if subsection does not lie completely
;                    within the original image (0b otherwise)
;
; MODIFICATION HISTORY:
;       Created:  01/18/10, TPEB -- Re-written (as get_subsection.pro)
;                                   from earlier versions.
;       Modified: 07/09/10, TPEB -- Name change, and made robust
;                                   against going off the edge of the
;                                   input image.
;-

FUNCTION EXTRACT_SUBSECTION, array_in, start, subsize, elements, FLAG=flag
  
  ;;=============================================================
  ;; Check for size of the input array & negative starting points
  ;;=============================================================
  
  flag = 0b
  
  ;; Create empty array for retrun
  array_out = dblarr(subsize)
  blank = lonarr(2)
  
  ;; Check to see if the subsection lies at all on the image
  insize = size(array_in, /DIM)
  lower = start + subsize - 1
  upper = insize - start
  
  ind = WHERE(lower LT 0, nlow)
  ind = WHERE(upper LT 0, nupp)
  
  ;; If requested subsection is not part of image, return blank array
  IF (nlow NE 0) OR (nupp NE 0) THEN BEGIN
     message,'Requested subsection does not lie on input image!',/inf
     flag = 5b
     RETURN,array_out
  ENDIF
  
  
  ;; Negative Starting Values
  ind = WHERE( start LT 0, nneg )
  IF (nneg GT 0) THEN BEGIN
     FOR i=0L, nneg-1 DO BEGIN
        blank[ind[i]] = abs(start[ind[i]])
        subsize[ind[i]] += start[ind[i]]
        start[ind[i]] = 0
        flag = 1b
        ;;message,'Flag neg',/inf
     ENDFOR
  ENDIF
  
  ;; Too large ending values
  overrun = size(array_in, /DIM) - (start + subsize)
  ind = WHERE( overrun LT 0, nbig )
  IF (nbig GT 0) THEN BEGIN
     FOR i=0L, nbig-1 DO BEGIN
        subsize[ind[i]] += overrun[ind[i]]
        flag = 2b
        ;;message,'Flag big',/inf
     ENDFOR
  ENDIF
  
  ;;=============================================================
  ;; Set variables for start and end in i & j
  ;;=============================================================
  
  i0 = start[0]
  j0 = start[1]
  
  i1 = i0 + subsize[0] - 1
  j1 = j0 + subsize[1] - 1
  
  ;;===========================================================
  ;; Get subsection
  ;;===========================================================
  
  array_out[blank[0]:blank[0] + subsize[0]-1, blank[1]:$
            blank[1] + subsize[1]-1] = array_in[i0:i1, j0:j1]
  
  IF ARG_PRESENT( elements ) THEN BEGIN
     elements = lonarr(2,2)
     elements[0,0] = i0
     elements[0,1] = i1
     elements[1,0] = j0
     elements[1,1] = j1
  ENDIF
  
  RETURN, array_out
  
END
