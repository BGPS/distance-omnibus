;+
; NAME:
;       RANDHT
;
; PURPOSE:
;       RANDHT generates n observations distributed as some continous
;       heavy-tailed distribution. Options are power law, log-normal,
;       stretched exponential, power law with cutoff, and
;       exponential. Can specify lower cutoff, if desired.
;
;       This is an IDL implementation of randht.m found at
;       http://www.santafe.edu/~aaronc/powerlaws/
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       x = RANDHT(n, varargin)
;
; INPUTS:
;       N        -- The number of random deviates desired.
;       VARARGIN -- A variable number of input arguments; see below.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       X -- The output random deviate(s).
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; EXAMPLE:
;       x = randht(10000,'powerlaw',alpha);
;       x = randht(10000,'xmin',xmin,'powerlaw',alpha);
;       x = randht(10000,'cutoff',alpha, lambda);
;       x = randht(10000,'xmin',xmin,'cutoff',alpha,lambda);
;       x = randht(10000,'exponential',lambda);
;       x = randht(10000,'lognormal',mu,sigma);
;       x = randht(10000,'stretched',lambda,beta);
;
;       See also PLFIT, PLVAR, PLPVA
;
; LICENSE INFORMATION:
;       The randht.m from which this IDL routine was derived is
;       Copyright (C) 2007 Aaron Clauset (Santa Fe Institute)
;       Distributed under GPL 2.0
;       http://www.gnu.org/copyleft/gpl.html
;       RANDHT comes with ABSOLUTELY NO WARRANTY
;
;       This IDL version comes with even less warranty.
;
; MODIFICATION HISTORY:
;
;       Created:  07/18/13, TPEB -- Initial IDL version, derived from
;                                   randht.m V1.0.2 (2008 April) found
;                                   at http://tuvalu.santafe.edu/
;                                   ~aaronc/powerlaws/randht.m
;       Modified: 07/29/13, TPEB -- Added COMPILE_OPT statement.
;
;-

FUNCTION RANDHT, n, v1, v2, v3, v4, v5
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  type   = ''                   ;
  xmin   = 1                    ;
  alpha  = 2.5                  ;
  beta   = 1                    ;
  lambda = 1                    ;
  mu     = 1                    ;
  sigma  = 1                    ;
  
  ;; Parse command-line parameters; trap for bad input
  i = 1
  nvar = n_params()-1
  CASE nvar OF
     1:   v = {v1:v1}
     2:   v = {v1:v1, v2:v2}
     3:   v = {v1:v1, v2:v2, v3:v3}
     4:   v = {v1:v1, v2:v2, v3:v3, v4:v4}
     5:   v = {v1:v1, v2:v2, v3:v3, v4:v4, v5:v5}
     ELSE: message,'Type of distribution is required!'
  ENDCASE
  
  WHILE i LE nvar DO BEGIN
     argok = 1b
     IF size(v.(i-1),/TYPE) EQ 7 THEN BEGIN
        CASE strlowcase(v.(i-1)) OF
           
           'xmin' : BEGIN
              xmin = double(v.(i))
              i++
           END
           
           'powerlaw' : BEGIN
              type  = 'PL'
              alpha = double(v.(i))
              i++
           END
           
           'cutoff' : BEGIN
              type   = 'PC'
              alpha  = double(v.(i))
              lambda = double(v.(i+1))
              i += 2
           END
           
           'exponential' : BEGIN
              type   = 'EX'
              lambda = double(v.(i))
              i++
           END
           
           'lognormal' : BEGIN
              type  = 'LN'
              mu    = double(v.(i))
              sigma = double(v.(i+1))
              i += 2
           END
           
           'stretched' : BEGIN
              type   = 'ST'
              lambda = double(v.(i))
              beta   = double(v.(i+1))
              i += 2
           END
           
           ELSE : argok = 0b
           
        ENDCASE
     ENDIF
     
     IF ~argok THEN message,'Ignoring invalid argument #'+$
                            string(i+1,format="(I0)"),/cont
     i++
  ENDWHILE
  
  ;; Check for scalar values
  IF n_elements(n) NE 1 || n LT 1 THEN BEGIN
     message,'Error: invalid "n" argument; using default.',/cont
     n = 10000
  ENDIF
  IF n_elements(xmin) NE 1 || xmin LT 1 THEN BEGIN
     message,'Error: invalid "xmin" argument; using default.',/cont
     xmin = 1
  ENDIF
  
  undefine,seed
  ;;=============================================
  ;; Generate random numbers based on input type
  CASE type OF
     
     'EX' : BEGIN
        x = xmin - (1.d / lambda)*alog(1.d - randomu(seed,n))
     END
     
     'LN' : BEGIN
        y = exp(mu + sigma * randomu(seed, 10*n, /NORM))
        WHILE 1b DO BEGIN
           y = y[where(y GE xmin)]
           q = n_elements(y) - n
           IF q EQ 0 THEN BREAK
           IF q GT 0 THEN BEGIN
              r = permute(n_elements(y))
              y = y[r[q:*]]
              BREAK
           ENDIF
           IF q LT 0 THEN y = [y, exp(mu + sigma * randomu(seed, 10*n, /NORM))]
        ENDWHILE
        x = y
     END
     
     'ST' : $
        x = (xmin^beta - (1.d / lambda)*alog(1.d - randomu(seed,n)))^(1.d/beta)
     
     'PC' : BEGIN             
        x = !null
        y = xmin - (1.d / lambda) * alog(1.d - randomu(seed,10*n))
        WHILE 1b DO BEGIN
           ind = where(randomu(seed,10*n) LT (y/xmin)^(-alpha),nind)
           y = ~nind ? !null : y[ind] 
           x = [x, y]
           q = n_elements(x) - n
           IF q EQ 0 THEN BREAK
           IF q GT 0 THEN BEGIN
              r = permute(n_elements(y))
              y = y[r[q:*]]
              BREAK
           ENDIF
           IF q LT 0 THEN $
              y = xmin - (1.d / lambda) * alog*(1.d - randomu(seed,10*n))
        ENDWHILE
        
     END
     
     ELSE : x = xmin * (1.d - randomu(seed,n))^(-1.d  / (alpha - 1.d))
     
  ENDCASE
     
  RETURN,x
END
