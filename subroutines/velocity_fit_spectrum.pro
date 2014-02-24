;+
; NAME:
;       VELOCITY_FIT_SPECTRUM
;
; PURPOSE:
;       Fit dense gas velocity spectra related to the distance-omnibus
;       project.  Routine returns VLSR as well as the (self-absorbed?)
;       fit to the spectrum.
;
; CATEGORY:
;       distance-omnibus subroutine
;
; CALLING SEQUENCE:
;       Tfit = VELOCITY_FIT_SPECTRUM(v [,/HCOP][,/N2HP][,/CS21][VLSR=vlsr])
;
; INPUTS:
;       V -- Element from the BGPS velocity structure
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       (One required!)
;       HCOP -- Do the HCO+ spectra
;       N2HP -- Do the N2H+ spectra
;       CS21 -- Do the CS(2-1) spectra
;
;       VERBOSE -- Be (more) verbose with output.
;
; OUTPUTS:
;       TFIT -- The fit to the spectrum
;
; OPTIONAL OUTPUTS:
;       VLSR    -- The centroid of the Gaussian fit
;       IS_SPEC -- Returns 1b for extant spectrum, 0b otherwise
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  11/09/11, TPEB -- Initial Version
;       Modified: 06/21/12, TPEB -- Fixed bug that caused SNDI to be
;                                   -NaN because of DV < 0.
;       Modified: 02/25/13, TPEB -- Added MAX optional output.
;
;-

FUNCTION VELOCITY_FIT_SPECTRUM, v, VLSR=vlsr, HCOP=hcop, N2HP=n2hp, CS21=cs21,$
                                IS_SPEC=is_spec, FA=fa, SN=sn, MAX=max, $
                                VERBOSE=verbose
  
  ;; Check for inputs
  IF n_elements(v) NE 1 THEN BEGIN
     message,'Error: Requires input of single element of BGPS_VELCOITY_STRUCT',$
             /cont
     vlsr = -1000.d
     RETURN,0
  ENDIF
  
  ;; Set default 'fail' values for return
  tfit    = v.v_std*0.d
  vlsr    = -1000.d
  is_spec = 0b
  FA      = [0.d,0.d,0.d]
  sn      = 0.d
  
  ;; Parse Spectral Lines
  hcop = KEYWORD_SET(hcop)
  n2hp = KEYWORD_SET(n2hp)
  cs21 = KEYWORD_SET(cs21)
  IF (hcop+n2hp+cs21) NE 1 THEN BEGIN
     message,'Error: Need to select a single spectral line',/cont
     RETURN,tfit
  ENDIF
  IF hcop THEN line = 'hcop'
  IF n2hp THEN line = 'n2hp'
  IF cs21 THEN line = 'cs21'
  
  ;; Check for # of spectra
  command = 'n = v.n_'+line
  errcode = execute(command)
  IF n NE 0 THEN BEGIN
     is_spec = 1b
     
     ;; Make weighted average of the extant spectra
     command = 'fns = strsplit(v.'+line+'_fn," ",COUNT=nfn,/EXTRACT)'
     errcode = execute(command)
     FOR is=0L, n-1 DO BEGIN
        ispec = baseline_spec(readfits(fns[is],hdr,/silent))
        ivel  = v_from_hdr(hdr,DV=dv)
        irms  = rms_dg_spec(ivel,ispec)
        max = max(ispec)
        
        IF is EQ 0 THEN BEGIN
           spec = ispec / (irms * irms) 
           wght = 1.d   / (irms * irms)
        ENDIF ELSE BEGIN
           spec += ispec / (irms * irms)
           wght += 1.d   / (irms * irms)
        ENDELSE
     ENDFOR
     
     ;; Put it together into 'standard' variables
     spec /= wght
     vstd = ivel
     rms = rms_dg_spec(vstd,spec)
     snd = max(spec) / rms
     
     ;; For statistics, look only at the 'narrow' region 
     ;;    vstd = [-100,200] km/s
     vind = WHERE(vstd GE -100. AND vstd LE 200., nv)
     nvstd = vstd[vind]
     nspec = spec[vind]
     
     ;; Find points meeting 3 * rms
     sig_ind = WHERE(nspec GE 3.d*rms, n_sig)
     
     ;; Only continue for spectra with 2 or more data points > 3sig
     IF n_sig GE 2. THEN BEGIN
        vs = nvstd[sig_ind]
        ss = nspec[sig_ind]
        
        ;; Do the moment calculation on all the data points above
        ;;    3sig in order to estimate v_cen
        mom = moments_spectra(vs,ss)
        
        ;; Set Gaussian Fitting Parameters
        parinfo = replicate( {value:0.d, limited:[1b,1b], limits:[0.d,0.d]}, 3 )
        parinfo[0].limited = [1b,0b]
        parinfo[1].limits  = [-100,200]
        parinfo[2].limits  = [0.47,4.25]
        
        ;; Gaussian estimate, max, v_cen, FWHM ~ 3.5 km/s (slightly
        ;;    wider than Wayne's median)
        parinfo.value = [max(nspec), mom[1], 1.5]
        
        ;; Do a preliminary Gaussian fit using gest as the starting
        ;;   point, force POSITIVE solution
        A = MPFITFUN('gauss2',nvstd,nspec,nspec*0.+rms,PARINFO=parinfo,$
                         ;;ESTIMATES=parinfo.value,/POSITIVE,$
                         ERRMSG=errmsg, STATUS=status,/QUIET,$
                         CHISQ=chisq,DOF=dof)
        yfit = gauss2(nvstd,A)
        IF status EQ 0 THEN BEGIN
           print,'(a) BGPS #'+string(v.cnum,format="(I4)")+'  '+errmsg
           print,parinfo.value
        ENDIF
        ;; Calculate preliminary S/N ratios based on T_mb fit
        snf = A[0] / rms
        
        ;; If the two values do not correspond well, try refitting
        ;;    based on MAX(nspec), same as above but with different
        ;;    Gaussian Estimators.
        IF (snf LT snd*0.8) OR (snf LT 3.) OR (snf GT snd*1.5) THEN BEGIN
           maxt = max(nspec, mind)
           maxv = (nvstd[mind])[0]
           parinfo.value = [maxt,maxv,1.5]
           A = MPFITFUN('gauss2',nvstd,nspec,nspec*0.+rms,PARINFO=parinfo,$
                            ;;ESITMATES=parinfo.value,/POSITIVE,$
                            ERRMSG=errmsg, STATUS=status,/QUIET,$
                            CHISQ=chisq,DOF=dof)
           yfit = gauss2(nvstd,A)
           IF status EQ 0 THEN BEGIN
              print,'(b) BGPS #'+string(v.cnum,format="(I4)")+'  '+$
                    strupcase(line)+'  '+errmsg
              print,parinfo.value
              print,parinfo.limits
              print,parinfo.limited
           ENDIF
           snf = A[0] / rms
        ENDIF
        
        ;; For sources with snd >= 10, use SA fitter (sources less
        ;;     than this detection level may not be fit well).
        IF snd GE 10. THEN BEGIN
           undefine,params
           sa_fit = fit_sa_spectrum(vstd, spec, params, EST=A, /QUIET, $
                                    RMS=rms, ERRMSG=errmsg, STATUS=status)
           IF status EQ 0 THEN print,'Problem with fit_sa_spectrum'
           A = params[0:2]
        ENDIF 
        
        ;; Recompute moments based on Gaussian fit: window = vc +-
        ;;   2sig (or 3km/s, whichever is bigger)
        width = (2.d*A[2] > 3.d)
        momwind = WHERE(vstd GE A[1]-width AND vstd LE A[1]+width,nw)
        IF nw EQ 0 THEN BEGIN
           print,A
           message,'Houston, we have a problem!'
        ENDIF
        
        ;; Use moments to calculate the integrated intensity S/N
        mom  = moments_spectra(vstd[momwind],spec[momwind])
        rmsi = rms * sqrt(abs(dv) * (max(vstd[momwind])-min(vstd[momwind])))
        sndi = mom[0] / rmsi
        
        ;; Use sndi to decide what to return...
        IF sndi GE 6. THEN BEGIN
           tfit = gauss2(v.v_std,A) 
           FA = A
           sn = sndi
           vlsr = A[1]
           IF KEYWORD_SET(verbose) THEN $
              print,string(strupcase(line),A,format=$
                           "(A0':  ',F5.2,'  ',F6.2,'  ',F5.2)")
        ENDIF
     ENDIF 
  ENDIF 
  
  RETURN,tfit
END
