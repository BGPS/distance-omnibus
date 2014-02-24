;+
; NAME:
;       STR_DMS2DEG
;
; PURPOSE:
;       Convert a string  coordinate value in DMS format into a
;       decimal degrees format.
;
; CATEGORY:
;       Coordinate utility
;
; CALLING SEQUENCE:
;       deg = STR_DMS2DEG( dms_string [,/RA] )
;
; INPUTS:
;       DMS_STRING -- String containing DMS coordinate in format
;                     "DD:MM:SS.SS" or "HH:MM:SS.SSS"
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       RA         -- For R.A. Coordinates, the decimal hours are
;                     converted to decimal degrees.
;
; OUTPUTS:
;       DEG        -- Output coordinate in degrees
;
; OPTIONAL OUTPUTS:
;       NONE
;
; EXAMPLE:
;       deg = str_dms2deg('14:32:34.6',/RA)
;
; MODIFICATION HISTORY:
;
;       Created:  02/14/11, TPEB -- Initial version, improving upon
;                                   existing personal code
;-

FUNCTION STR_DMS2DEG, string, RA=ra, DEBUG=debug
  
  one = STRPOS(string, ':')
  first = STRTRIM(STRMID(string,0,one),2)
  IF STRCMP(first[0],'-',1) THEN sign = -1 ELSE sign = 1
  d = double( first )
  
  two = STRPOS(string, ':', one+1)
  m = double( STRMID(string, one+1, two-one+1) )
  
  three = STRPOS(string, ':', two+1)
  s = double( STRMID(string, two+1) )
  
  deg =  sign*(abs(d)+abs(m)/60.+abs(s)/3600.)
  
  IF KEYWORD_SET( ra ) THEN deg *= 15.d
  
  RETURN,deg
END
