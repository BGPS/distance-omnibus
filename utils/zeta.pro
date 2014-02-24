;+
; NAME:
;       ZETA
;
; PURPOSE:
;       Riemann Zeta function
;
;
; CATEGORY:
;       Mathematical Utility
;
; CALLING SEQUENCE:
;       f = ZETA( z )
;
; INPUTS:
;       Z -- Complex number of any size.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       F -- The Riemann Zeta function
;
; OPTIONAL OUTPUTS:
;       NONE
;
; PROCEDURE:
;       This program calculates the Riemann Zeta function for the
;       elements of Z using the Dirichlet deta function. Z may be
;       complex and any size. Best accuracy for abs(z)<80. 
;
;       Has a pole at z=1, zeros for z=(-even integers), infinite
;       number of zeros for z=1/2+i*y 
;
; MODIFICATION HISTORY:
;
;       Created:  03/24/13, TPEB -- Initial IDL version, derived from
;                                   zeta.m and deta.m by Paul Godfrey
;                                   (pgodfrey@conexant.com) 3-24-01.
;
;-

;; Function DETA derived from deta.m
;;      Calculates Dirichlet functions of the form
;;
;;      f = sum((-1)^n/(k*n+1)^z)
;;
;;      over the entire complex plane Z may be complex and any size
;;      Best accuracy for Abs(z) < 100
;;
;;      Usage: f = deta(z)
;;         or  f = deta(z,k)
;;
;;      where k determines which Dirichlet function to sum
;;      For Eta (Zeta, Lambda):   k=1
;;      For Betad: k=2
;;
;;      This function can use a LOT of memory when size(z) is
;;      large. Consider using the Memory and Pack commands. Also,
;;      consider breaking z up into smaller chunks. 
;;
;;      Requires a complex Gamma routine.
;;  Paul Godfrey
;;  pgodfrey@conexant.com
;;  March 24, 2001
FUNCTION DETA, z, k
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Parse input & error check
  IF ~n_elements(k) THEN k = 1
  k = k[0]
  IF k LT 1 || k GT 2 THEN $
     message,'Warning: Unknown function being calculated!  '+$
             'Results valid only for Real(z)>0.5',/cont
  sizz = size(z,/DIM)
  rz = real_part(z)
  iz = imaginary(z)
  
  ;; Determine which parts of z correspons to zero, even, odd parts
  iszero = where(z EQ 0, nzero)
  iseven = where(z EQ (round(z/2.)*2.) AND rz LT 0 AND iz EQ 0, neven)
  isodd  = where(z EQ (round(z-1)/2.*2.+1.) AND rz LT 0 AND iz EQ 0, nodd)
  
  L = where(rz LT 0.5,nl)
  IF nl THEN $
     z[L] = 1. - z[L]
  
  ;; Series coefficients were calculated using 
  ;; c(m)=sum from n=m to 64 of (binomial(n,m)/2^n) for m=0:64
  
  ;; coefficients are symmetrical about the 0.5 value. Each pair sums to +-1
  ;; abs(coefficients) look like erfc(k*m)/2 due to binomial terms
  ;; sum(cm) must = 0.5 = eta(0) = betad(0)
  ;; worst case error occurs for z = 0.5 + i*large
  
  cm= [ .99999999999999999997d,$
        -.99999999999999999821d,$
        .99999999999999994183d,$
        -.99999999999999875788d,$
        .99999999999998040668d,$
        -.99999999999975652196d,$
        .99999999999751767484d,$
        -.99999999997864739190d,$
        .99999999984183784058d,$
        -.99999999897537734890d,$
        .99999999412319859549d,$
        -.99999996986230482845d,$
        .99999986068828287678d,$
        -.99999941559419338151d,$
        .99999776238757525623d,$
        -.99999214148507363026d,$
        .99997457616475604912d,$
        -.99992394671207596228d,$
        .99978893483826239739d,$
        -.99945495809777621055d,$
        .99868681159465798081d,$
        -.99704078337369034566d,$
        .99374872693175507536d,$
        -.98759401271422391785d,$
        .97682326283354439220d,$
        -.95915923302922997013d,$
        .93198380256105393618d,$
        -.89273040299591077603d,$
        .83945793215750220154d,$
        -.77148960729470505477d,$
        .68992761745934847866d,$
        -.59784149990330073143d,$
        .50000000000000000000d,$
        -.40215850009669926857d,$
        .31007238254065152134d,$
        -.22851039270529494523d,$
        .16054206784249779846d,$
        -.10726959700408922397d,$
        .68016197438946063823d-1,$
        -.40840766970770029873d-1,$
        .23176737166455607805d-1,$
        -.12405987285776082154d-1,$
        .62512730682449246388d-2,$
        -.29592166263096543401d-2,$
        .13131884053420191908d-2,$
        -.54504190222378945440d-3,$
        .21106516173760261250d-3,$
        -.76053287924037718971d-4,$
        .25423835243950883896d-4,$
        -.78585149263697370338d-5,$
        .22376124247437700378d-5,$
        -.58440580661848562719d-6,$
        .13931171712321674741d-6,$
        -.30137695171547022183d-7,$
        .58768014045093054654d-8,$
        -.10246226511017621219d-8,$
        .15816215942184366772d-9,$
        -.21352608103961806529d-10,$
        .24823251635643084345d-11,$
        -.24347803504257137241d-12,$
        .19593322190397666205d-13,$
        -.12421162189080181548d-14,$
        .58167446553847312884d-16,$
        -.17889335846010823161d-17,$
        .27105054312137610850d-19]
  
  cm   = reverse(cm)            ; sum from small to big
  nmax = n_elements(cm)
  n    = matlab_colon(1,k,k*nmax)
  n    = reverse(n)
  
  ;; print,k,nmax,k*nmax
  ;; print,m4_stat(cm)
  ;; print,m4_stat(n)
  
  ;; z is a  LR vector
  ;; n is an UD vector
  ;; IDL mash-up of MATLAB meshgrid routine
  capZ = cmreplicate(z,n_elements(n))
  capN = transpose(cmreplicate(n,n_elements(z)))
  
  ;; This can take a LOT of memory
  f = cm ## (capN^(-capZ))
  ;; but it's really fast to form the series expansion N.^-Z
  ;; and then sum it by an inner product cm*()  :)
  
  ;; Reflect across 1/2
  IF nl THEN BEGIN
     zz = z[L]
     IF k EQ 1 THEN BEGIN
        ;; Eta function reflection
        ;; for test: deta(1,1) should = alog(2)
        t = (2.d - 2.d^(zz+1.d))/(2.d^zz-2.d)/!dpi^zz
        f[L] = t * cos(!dpi/2.d*zz) * gamma(zz) * f[L]
        IF nzero THEN f[iszero] = 0.5d
        IF neven THEN f[iseven] = 0.d
     ENDIF
     IF k EQ 2 THEN BEGIN
        ;; Betad function reflection
        ;; for test: deta(0,2) should = 0.5
        ;; for test: deta(1,2) should = pi/4
        f[L] = (2.d/!dpi)^zz*sin(!dpi/2.*zz)*gamma(zz)*f[L]
        IF nodd THEN f[isodd] = 0.d
     ENDIF
     IF k GT 2 THEN BEGIN
        ;; Insert reflection formula for other Dirichlet functions here
        f[L] = (1.d/!dpi)^zz*gamma(zz)*f[L]
        f[L] = !values.d_nan
     ENDIF
  ENDIF
  
  RETURN,f
END

FUNCTION ZETA, z
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  zz = 2.d^z
  k = zz/(zz-2.d)
  
  f = k*deta(z,1)
  
  p = where(z EQ 1,np)
  IF np THEN f[p] = !values.d_infinity
    
  RETURN,f
END
