FUNCTION Blank_Axis, axis, index, number
  
RETURN,''
  ;; ;; A special case.
  ;; IF number EQ 0 THEN RETURN, '0' 
  
  ;; ;; Assuming multiples of 10 with format.
  ;; ex = String(number, Format='(e8.1)') 
  ;; pt = StrPos(ex, '.')
  
  ;; first = StrMid(ex, 0, pt+2)
  ;; sign = StrMid(ex, pt+3, 1)
  ;; thisExponent = StrMid(ex, pt+4)
  
  ;; ;; Shave off leading zero in exponent
  ;; WHILE StrMid(thisExponent, 0, 1) EQ '0' DO $
  ;;    thisExponent = StrMid(thisExponent, 1)
  
  ;; ;; Fix for sign and missing zero problem.
  ;; IF (Long(thisExponent) EQ 0) THEN BEGIN
  ;;    sign = ''
  ;;    thisExponent = '0'
  ;; ENDIF
  
  ;; ;; Make the exponent a superscript.
  ;; IF sign EQ '-' THEN BEGIN
  ;;    RETURN, '10!U' + sign + thisExponent + '!N' 
  ;; ENDIF ELSE BEGIN             
  ;;    RETURN, '10!U' + thisExponent + '!N'
  ;; ENDELSE
  
END
