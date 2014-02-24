;+
; NAME:
;       MATLAB_COLON
;
; PURPOSE:
;       IDL implementation of the MATLAB colon operator for creating
;       arrays
;
; CATEGORY:
;       MATLAB compatability routine
;
; CALLING SEQUENCE:
;       res = MATLAB_COLON( start, step, rear [,/FLOAT][,/LONG][,/INT] )
;       res = MATLAB_COLON( start, rear [...])
;
; INPUTS:
;       START -- Starting value
;       STEP  -- Step between values (Optional: Default = 1)
;       REAR  -- Ending value of sequence
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       DOUBLE -- Return value is type DOUBLE
;       FLOAT  -- Return value is type FLOAT [Default: double]
;       LONG   -- Return value is type LONG
;       INT    -- Return value is type INT
;
; OUTPUTS:
;       RES -- Return vector
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  03/28/13, TPEB -- Initial version.
;       Modified: 07/19/13, TPEB -- To conform with MATLAB use, may
;                                   omit STEP input, specifying only
;                                   START and REAR.
;
;-

FUNCTION MATLAB_COLON, start, step, rear, FLOAT=float, INT=int, LONG=long, $
                       DOUBLE=double, VERBOSE=verbose
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  ON_ERROR, 2
  
  ;; Parse keyword input
  IF KEYWORD_SET(int) THEN type='INT' ELSE $
     IF KEYWORD_SET(long) THEN type='LONG' ELSE $
        IF KEYWORD_SET(int) THEN type='FLOAT' ELSE $
           type='DOUBLE'
  IF KEYWORD_SET(double) THEN type='DOUBLE'
  verbose = KEYWORD_SET(verbose)
  
  ;; Check whether step is specified
  IF ~n_elements(rear) THEN BEGIN
     rear = step
     step = 1
  ENDIF
  
  ;; Compute maximum number of return values needed
  nstep = fix(round((double(rear)-double(start))/double(step)+1.d))
  IF verbose THEN print,'NSTEP: ',nstep
  
  ;; Type
  CASE type OF
     'DOUBLE' : vec = dindgen(nstep) * double(step) + double(start)
     'FLOAT'  : vec = findgen(nstep) * float(step)  + float(start)
     'LONG'   : vec = lindgen(nstep) * long(step)   + long(start)
     'INT'    : vec = indgen(nstep)  * fix(step)    + fix(start)
  ENDCASE
  
  ;; Check for out of bounds
  IF step GT 0 THEN BEGIN
     ki = where(vec LE (rear + step/2.), nk)
     vec = vec[ki]
  ENDIF ELSE BEGIN
     ki = where(vec GE (rear - step/2.), nk)
     vec = vec[ki]
  ENDELSE
  
  RETURN,vec
END
