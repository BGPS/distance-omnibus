;+
; NAME:
;       FDR_WIDTH
;
; PURPOSE:
;       Computes a histogram bin width according to the Freedman &
;       Diasonis Rule (FDR; Freedman & Diaconis 1981, Probability
;       Theory and Related Fields, 57, 453).
;
; CATEGORY:
;       Statistics
;
; CALLING SEQUENCE:
;       w = FDR_WIDTH( x )
;
; INPUTS:
;       X = Input data array for which to compute the histogram width
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  05/06/13, TPEB -- Initial version, based on
;                                   Wikipedia description and code
;                                   from cgBoxPlot.pro.
;
;-

FUNCTION FDR_WIDTH, data
  
  ;; Compute the Inner Quartile Range
  sortedData = data[Sort(data)]
  n = N_Elements(sortedData)
  IF n MOD 2 EQ 0 THEN BEGIN
     index = n/2
     medianData = (sortedData[index-1] + sortedData[index]) / 2.0
     lowerGroup = sortedData[0:index-1]
     higherGroup = sortedData[index:n-1]
  ENDIF ELSE BEGIN ; The middle point belongs to both upper and lower quartiles.
     index = n/2
     medianData = sortedData[index]
     lowerGroup = sortedData[0:index]
     higherGroup = sortedData[index:n-1]
  ENDELSE
  quartile_25 = Median(lowerGroup, /EVEN)
  quartile_75 = Median(higherGroup, /EVEN) 
  ;; Calculate IQR
  iqr = quartile_75 - quartile_25
  
  w = 2.d * iqr * n^(-1.d/3.d)
  
  RETURN, w
END
