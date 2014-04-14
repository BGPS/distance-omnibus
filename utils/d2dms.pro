function d2dms,degrees,dmsstr,ndp,dchar
;
;+
;				D2DMS
;
; Convert decimal degrees (or hours) to degrees (hours), minutes and seconds.
;
; CALLING SEQUENCE:
;	dms=d2dms(degrees [,dmsstr [,ndp [,dchar]]])
;
; INPUTS:
;	degrees - decimal degrees or hours
;	ndp - number of digits after decimal point to include in dmsstr
;	      (default is 3)		
;	dchar - delimiting character used between degrees, minutes and seconds
;		in dmsstr (default is <space>).
; OUTPUTS:
;	dms - 3 element f.p. array of degrees, minutes, seconds
;	dmsstr - string containing degrees, minutes and seconds, e.g.:
;		 -237 23 32.123, or 22:43:16.21 (ndp=2, dchar=':').
;
; HISTORY:
;	version 1.0  G. Hartig  Mar. 1990
;-
;-------------------------------------------------------------------------------
;
np=n_params(0)
if np lt 3 then ndp=3
if np lt 4 then dchar=' '
;
dms=fltarr(3)
if degrees lt 0 then sign='-' else sign=''
degrees=abs(degrees)
deg=fix(degrees)
dms(0)=float(deg)
minutes=(degrees-deg)*60.
min=fix(minutes)
dms(1)=float(min)
seconds=(minutes-min)*60.
sec=fix(seconds)
dms(2)=seconds
fsec=seconds-sec
if sign eq '-' then dms=-dms
;
if np gt 1 then begin
  sd=strtrim(string(deg,format='(i3)'),1)
  sm=strtrim(string(min,format='(i2)'),1)
  if strlen(sm) lt 2 then sm='0'+sm
  ss=strtrim(string(sec,format='(i2)'),1)
  if strlen(ss) lt 2 then ss='0'+ss
  fst=strtrim(string(fsec),2)
  pp=strpos(fst,'.')	; this to accomodate both vax and sun versions
  fs=strmid(fst,pp,ndp+1)
  dmsstr=sign+sd+dchar+sm+dchar+ss+fs
endif
;
return,dms
end
