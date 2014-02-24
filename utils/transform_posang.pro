;+
; NAME:
;       TRANSFORM_POSANG
;
; PURPOSE:
;       Translate a position angle from one coordinate system to
;       another (i.e. a posang in RA/Dec coords to (l,b) coords).
;
; CATEGORY:
;       Astrometry Utility
;
; CALLING SEQUENCE:
;       result = TRANSFORM_POSANG( ANGLE, SELECT [,/RADIAN] )

; INPUTS:
;       ANGLE  -- The position angle to be transformed
;       AI     -- Longitude of point about which ANGLE is measured.
;                 In DEGRESS unless /RADIAN is set.
;       BI     -- Input Latitude in DEGREES
;
;       SELECT   From          To        |   SELECT      From            To
;        1     RA-Dec (2000)  Galactic   |     4       Ecliptic      RA-Dec    
;        2     Galactic       RA-DEC     |     5       Ecliptic      Galactic  
;        3     RA-Dec         Ecliptic   |     6       Galactic      Ecliptic  
;
; OPTIONAL INPUTS:
;      NONE
;
; KEYWORD PARAMETERS:
;      /RADIAN - if set, then all input and output angles are in
;                radians rather than degrees.
;
; OUTPUTS:
;      RESULT -- Output position angle in the new coordinate system.
;
; OPTIONAL OUTPUTS:
;      NONE
;
; USES ROUTINES:
;      EULER, GCIRC, POSANG
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;      Created:  08/12/10 -- TPEB, Initial Version, styled somewhat
;                            after EULER
;
;-
FUNCTION TRANSFORM_POSANG, angle, AI, BI, select, RADIAN=radian
  
  ;;select = 1
  ;;posang = 0.
  
  ;; The North pole in the orignal coordinate system (starting point for
  np_alp = 0.
  np_del = +90.
  
  ;; Coordinate transformations of both coordinate and North Pole
  EULER, AI,     BI,     l,   b,   select
  EULER, np_alp, np_del, l_n, b_n, select
  
  
  ;;POSANG, 1, AI/15., BI, np_alp/15., np_del, posang1
  POSANG, 1, l/15,   b,  l_n/15,     b_n,    posang2
  
  ;;print,l_n,b_n ,posang1,posang2

  posang = angle + posang2
  
  ;;Make sure output makes sense (i.e. [0,360])
  indlt = WHERE( posang LT 0,    nlt )
  indgt = WHERE( posang GT 360., ngt )
  
  IF nlt GT 0 THEN posang[indlt] += 360.
  IF ngt GT 0 THEN posang[indgt] -= 360.
  
  RETURN,posang
END


