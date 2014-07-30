;+
; NAME:
; 
;DLIB
;
; PURPOSE:
; 
;A cheesy alias to DOC_LIBRARY, with a name that's easier to
;type.
;
; CATEGORY:
; 
;Cheesy aliases.
;
; MODIFICATION HISTORY:
; 
;D. L. Windt, Bell Labs, April 1990.
;windt@bell-labs.com
;-

pro dlib, name, print=printflg, directory = direct, multi = multi
doc_library, name, print=printflg, directory = direct, multi = multi
return
end
