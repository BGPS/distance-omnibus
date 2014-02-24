;+
; NAME:
;       MYDEVICE
;
; PURPOSE:
;       Shortcut for writing EPS plots
;
; CATEGORY:
;       Cheap shortcut
;
; CALLING SEQUENCE:
;       MYDEVICE, filename [,FILENAME=filename][,XSIZE=xsize][,YSIZE=ysize]
;                 [,CT=ct][,/REVERSE][,/CMYK]
;
; INPUTS:
;       FILENAME -- Name of the output .eps file to be created
;
; OPTIONAL INPUTS:
;       XSIZE    -- Horizontal size of the output .eps file in inches (default =
;                   7 in)
;       YSIZE    -- Vertical size of the output .eps file in inches (default =
;                   5 in)
;       CT       -- Color table to be loaded (Default = none)
;       FILENAME -- Name of the output .eps file to be created (may be
;                   entered as regular or keyword input).
;
; KEYWORD PARAMETERS:
;       REVERSE  -- Reverse the colortable specified with CT.
;       CMYK     -- Use CMYK colors for PostScript output rather than
;                   RGB colors.  [DEFAULT]
;       RGB      -- Use RGB colors for PostScript output rather than
;                   CMYK colors.
;
; OUTPUTS:
;       NONE (opens filename for writing)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  06/29/10, TPEB -- Initial Version
;       Modified: 10/22/10, TPEB -- Added to distance-omnibus, and
;                                   made parsing of keywords better.
;       Modified: 06/28/11, TPEB -- Added filename as regular input,
;                                   because I got sick of typing
;                                   'filename='
;       Modified: 07/14/11, TPEB -- Added error checking.
;       Modified: 04/09/12, TPEB -- Added /REVERSE keyword in
;                                   conjunction with cgLoadct.
;       Modified: 07/19/12, TPEB -- Added CMYK keyword to force
;                                   PostScript output to be in CMYK
;                                   colors rather than RGB.
;       Modified: 11/14/12, TPEB -- Added RGB keyword, and changed
;                                   default behavior to always produce
;                                   CMYK unless /RGB.
;       Modified: 12/11/12, TPEB -- Added /BW option to force creation
;                                   of monotone PostScript output,
;                                   needed for Journal submission.
;       Modified: 05/05/13, TPEB -- Added HIDDEN COMPILE_OPT and code
;                                   cleanup related to
;                                   LOGICAL_PREDICATE.
;
;-

PRO MYDEVICE, filename, FILENAME=fn2, XSIZE=xsize, YSIZE=ysize, CT=ct, $
              REVERSE=reverse, CMYK=cmyk, RGB=rgb, BW=bw
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ON_ERROR,2
  
  cmyk = KEYWORD_SET(cmyk) || ~KEYWORD_SET(rgb)
  bw   = KEYWORD_SET(bw)
  
  IF ~n_elements(filename) THEN filename=fn2
  IF ~n_elements(filename) THEN $
     message,'ERROR: Must supply a filename to open for writing.'
  
  IF ~n_elements(xsize) THEN xsize = 7
  IF ~n_elements(ysize) THEN ysize = 5
  IF bw THEN cgLoadct,0,/silent,REVERSE=reverse $
  ELSE IF n_elements(ct) THEN cgLoadct,ct,/silent,REVERSE=reverse
  
  device,filename=filename,/encapsul,/isolatin1,color=~bw,bits_per_pixel=8,$
         XSIZE=xsize, YSIZE=ysize, /INCHES, CMYK=cmyk
  
END

