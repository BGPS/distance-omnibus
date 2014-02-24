;+
; NAME:
;       KDIST_CLEM
;
; PURPOSE:
;       To return the distance to an object given l,b,v from the
;       Clemens (1985) rotation curve
;
; CALLING SEQUENCE:
;       dist = KDIST_CLEM (L, B, V)
;
; INPUTS:
;       L   -- Galactic Longitude (decimal degrees)
;       B   -- Galactic Latitude (decimal degrees)
;       V   -- Velocity w.r.t. LSR in km/s
;
; KEYWORD PARAMETERS:
;       /NEAR, /FAR -- Report the near/far kinematic distances for Q1
;                      and Q4 data.
;       RO, VO      -- Force values for galactocentric distance for
;                      sun and velocity of the LSR around the GC.
;                      Default to 8.4 kpc and 254 km/s (Reid et al., 2009)
;       RGAL        -- Named keyword containding galactocentric radius
;                      of sources. 
;       /DYNAMICAL  -- Use the dynamical definition of the LSR
;       /KINEMATIC  -- Use the kinematic definition of the LSR (default)
;
; OUTPUTS:
;       DIST -- the kinematic distance in units of R0 (defaults to pc).
;
; MODIFICATION HISTORY:
;
;       Fri Feb 27 00:47:18 2009, Erik <eros@orthanc.local>
;		 Adapted from kindist.pro
;       Modified: 04/20/11, TPEB -- Updated to use the GALACTIC_PARAMS
;                                   structure.
;       Modified: 04/27/11, TPEB -- Routine now gets Solar Peculiar
;                                   Motion from GALACTIC_PARAMS
;                                   structure.
;       Modified: 06/28/11, TPEB -- Documentation cleanup and
;                                   modifications to allow use of
;                                   various other rotation curves.
;       Modified: 08/30/11, TPEB -- Fixed how the routine was handling
;                                   objects outside the Solar Circle
;                                   (Q2, Q3, and special VLSR objects
;                                   in Q1 & Q4), plus debugging code
;                                   (commented out).  
;       Modified: 01/23/12, TPEB -- Conformal changes to combining
;                                   all VPHYS2VLSR routines into a
;                                   single function.
;       Modified: 03/03/12, TPEB -- Shortened routine name for
;                                   convenience.
;       Modified: 06/27/12, TPEB -- Changed method of finding 'root'
;                                   in match to Clemens curve from
;                                   NEWTON to TNMIN.
;       Modified: 07/02/12, TPEB -- Fixed minor bug in TNMIN
;                                   implementation where the fit gets
;                                   stuck at a flat maximum (d_tan).
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: made compatible with the new
;                                   framework.
;
;-

;; FX_ROOT Fitting function
FUNCTION KDIST_CLEM_FIT, d, L=l, B=b, V=v
  
  vlsr  = vphys2vlsr(l, b, d, VS=0., /CLEM)
  
  ;; Clean out the 'underflow' error messages
  junk = CHECK_MATH()
  RETURN,abs(vlsr-v)
  
END


;; Main Function
FUNCTION KDIST_CLEM, l, b, v, NEAR=near, FAR=far, R0=r0, V0=v0, $
                     RGAL=rgal, PLOT=plot, VERBOSE=verbose
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params config file
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  
  ;; Count how many objects we want to find the KIDST for.
  n_obj = n_elements(l)
  verbose = KEYWORD_SET( verbose )
  
  R0    = 8500.
  d     = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Just do all this in VLSR space!  Get VLSR as a function of distance
  
  ;; Find where vlsr = v
  IF KEYWORD_SET( plot ) THEN BEGIN
     vlsr  = vphys2vlsr(l, b, d, VS=0., /CLEM)
     plot,d/1.d3,vlsr-v,yr=[-150,150],/yst,$
          xtit='D!dhelio!n [kpc]',ytit='V!dLSR!n - V [km s!u-1!n]',$
          tit=string(l,b,format="('G',F06.2,F+05.2)")
     oplot,d/1.d3,vphys2vlsr(l,b,d,vs=mw.vs)-v,$
           linestyle=2,color=cgColor('Gold')
     vline,R0*cos(l*!dtor)/1.d3/(cos(b*!dtor))
     vline,mw.R0/1.d3*cos(l*!dtor)/(cos(b*!dtor)),color=cgColor('Gold')
     vline,0.,/horiz
     vline,kdist(l,b,v,/NEAR)/1.d3,color=cgColor('Dodger Blue')
     vline,kdist(l,b,v,/FAR )/1.d3,color=cgColor('Deep Pink')
  ENDIF
  
  ;; Do a FOR loop to deal with vector inputs for l,b
  neardist = fltarr(n_obj)
  fardist  = fltarr(n_obj)
  
  FOR k = 0L, n_obj-1 DO BEGIN
     gl = l[k]
     gb = b[k]
     gv = v[k]
     
     dtan = R0*cos(gl*!dtor)/(cos(gb*!dtor))
     
     ;; Be smart about choosing starting point, ELSE...
     ;;    Get estimate of FAR distance from Reid curve.
     farind = WHERE(d GE dtan, nfi)
     IF nfi NE 0 THEN BEGIN
        junk = min(v-vphys2vlsr(l, b, d[farind], VS=0., /CLEM),/ABS,mind) 
        gfar = d[farind[mind]]
     ENDIF ELSE gfar = kdist(gl,gb,gv,/FAR) + 0.5d3
     
     ;; TNMIN inputs
     functargs = {L:gl,B:gb,V:gv}
     parinfo = {value:gfar,limited:[1b,0b],limits:[dtan-0.5d3,0.],$
                tnside:0,parname:'KDIST',step:10}
     fardist[k] = TNMIN('KDIST_CLEM_FIT',FUNCTARGS=functargs,$
                        /AUTODERIVATIVE,PARINFO=parinfo, STATUS=status,$
                        ERRMSG=errmsg, EPSABS=1.d-4, /QUIET, NITER=niter)
     IF verbose && status LE 0 THEN message,$
        string(status,format="(I0,'  ')")+errmsg,/cont 
     
     ;; Be smart about choosing starting point, ELSE...
     ;;    Get estimate of NEAR distance from Reid curve.
     nearind = WHERE(d LE dtan, nni)
     IF nni NE 0 THEN BEGIN
        junk = min(v-vphys2vlsr(l, b, d[nearind], VS=0., /CLEM),/ABS,mind) 
        gnear = d[nearind[mind]]
     ENDIF ELSE gnear = kdist(gl,gb,gv,/NEAR) - 0.5d3
     
     ;; Check to see if near KDIST_R09 is PHYSICAL
     IF gnear LE dtan THEN BEGIN
        
        ;; TNMIN inputs
        parinfo.limited = [1b,1b]
        parinfo.limits  = [0., dtan+0.5d3]
        parinfo.value   = gnear
        neardist[k] = TNMIN('KDIST_CLEM_FIT',FUNCTARGS=functargs,$
                            /AUTODERIVATIVE,PARINFO=parinfo, STATUS=status,$
                            ERRMSG=errmsg, EPSABS=1.d-4, /QUIET, NITER=niter)
        IF verbose && status LE 0 THEN message,$
           string(status,format="(I0,'  ')")+errmsg,/cont 
        
     ENDIF ELSE neardist[k] = fardist[k]
     
  ENDFOR
  
  dproj = neardist*cos(b*!dtor)
  rgal = sqrt(dproj^2 + R0^2 - 2*dproj*R0*cos(l*!dtor))
  
  ;; If object is in Q2 or Q3, then (near = far) = far
  ind = where(abs(l-180) lt 90, ct)
  IF ct GT 0 THEN neardist[ind] = fardist[ind]
  
  dist = keyword_set(near) ? neardist : fardist
  
  RETURN, abs(dist)
END
