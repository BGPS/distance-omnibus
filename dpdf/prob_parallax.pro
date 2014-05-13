;+
; NAME: 
;       PROB_PARALLAX
;
; PURPOSE: 
;       Return probability vector for cloud distance based on a
;       lookup-table of maser parallax measurements.
;
; CATEGORY:
;       distance-omnibus DPDF Generation Routine
;
; CALLING SEQUENCE:
;       probability = PROB_PARALLAX(struct [,DVEC=dvec]
;                     [,CONSTRAIN=constrain]) 
;
; INPUTS:
;       STRUCT -- SURVEY source structure (see OMNI_READ_CATALOG.pro),
;                 which includes source longitude and latitude.
;
; OPTIONAL INPUTS:
;       DVEC   -- Vector of distances (if not specified, will build
;                 one with values from dpdf_params.conf)
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       PROBABILITY -- Distance Probability Density Function
;
; OPTIONAL OUTPUTS:
;       CONSTRAIN   -- Does this routine provide a constraint on the
;                      SURVEY source distance (1b) or return a uniform
;                      prior (0b)?
;
; COMMON BLOCKS:
;       OMNI_CONFIG    -- The set of configuration structures, read in
;                         from the config files in conffiles/
;       PARALLAX_BLOCK -- Contains the BeSSeL maser parallax
;                         measurements.
;       VEL_BLOCK      -- Contains information about the dense gas
;                         velocity spectra.
;
; MODIFICATION HISTORY:
;
;       Created:  09/15/11, TPEB -- Initial version (based heavily
;                                   upon PROB_LONLAT.pro).  Does
;                                   nothing but return uniform
;                                   probability -- placeholder for
;                                   future functionality.
;       Modified: 09/18/13, TPEB -- Brought into the modern ages.
;       Modified: 12/07/13, TPEB -- Make the routine do something!
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;       Modified: 02/14/13, TPEB -- Allow GRS 13CO vlsr for
;                                   association (bug fix), and correct
;                                   the BESSEL structure element for
;                                   parallax error.
;       Modified: 04/11/14, TPEB -- Adjusted the normalized 3D
;                                   distance to v^4 to account for the
;                                   turbulent structure function.
;
;-

FUNCTION PROB_PARALLAX, s, DVEC = dvec, CONSTRAIN = constrain
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON PARALLAX_BLOCK, bessel, adist
  COMMON VEL_BLOCK, v, v_std
   
  ;; Read in galactic-params, dpdf-params & ancillary config files
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  IF ~exist(ancil) THEN ancil = omni_read_conffile('./conffiles/ancillary.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE $
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Check to see that BeSSeL is loaded & compute ADIST for common block
  IF ~exist(bessel) THEN BEGIN
     bfn = './ancillary/bessel_parallaxes.sav'
     IF ~FILE_TEST(bfn,/READ) THEN omni_parse_parallax
     restore,bfn,/ver
  ENDIF
  IF ~exist(adist) THEN BEGIN
     dist  = 1.d3 / bessel.px             ; [pc] Distance to the maser
     adist = ancil.ppv_dis / dist / !dtor ; [degr] Angular association distance
  ENDIF
  
  ;; Parse structure into useable variables
  vel = v[where(v.cnum EQ s.cnum)]
  l   = s.glon
  b   = s.glat
  constrain = 0b
  
  
  ;;===================================================================
  ;; Check the (l,b) association based on ADIST and VLSR association
  gcirc, 2, l, b, bessel.l, bessel.b, dis
  
  ;; Choose VLSR from dense gas or GRS 13CO
  CASE 1 OF
     vel.vlsr GE -500.: vlsr = vel.vlsr                          ; OK dg vlsr
     where([1,2,5,6] EQ vel.grs.flag) NE -1: vlsr = vel.grs.vlsr ; OK GRS flag
     ELSE: vlsr = -1000.
  ENDCASE
  dv = abs(bessel.vlsr - vlsr)
  
  ind = where( (dis/3600. LE adist) AND (dv LE ancil.ppv_dv), n_parallax )
  
  ;; If nothing, then return uniform prior
  IF n_parallax EQ 0 THEN RETURN, fltarr(dpdfs.nbins) + 1.d0/(dpdfs.nbins)
  
  ;; Otherwise, cull arrays and compute...
  bes = bessel[ind]             ; BeSSeL structure
  dis = dis[ind]                ; Angular distance [arcsec] 
  dv  = dv[ind]                 ; Velocity separation [km/s]
  adb = adist[ind]              ; Angular association radius [deg]
  
  ;; Compute the normalized 3D distance...  sqrt(ang^2 + vel^4)
  n3d = sqrt((dis*dis/adb/adb/3600./3600.) + $
             (dv*dv*dv*dv/ancil.ppv_dv/ancil.ppv_dv/ancil.ppv_dv/ancil.ppv_dv))
  
  ;;===========================================
  ;; Build the DPDF
  
  pxarr = dindgen(10001) * 4.d-4 ; Parallax array
  dpdf  = d * 0.d                ; DPDF array
  
  ;; Loop through the number of parallaxs associated with this 
  ;;   catalog object. 
  FOR ii=0,n_parallax-1 DO BEGIN
     
     ;; Construct a Gaussian in PARALLAX space, corresponding to the
     ;; data from the BeSSeL table
     yarr = gauss_1(pxarr,[1.,bes[ii].px,bes[ii].e_px])
     
     ;; Interpolate this onto the DISTANCE scale, weighted by the
     ;;    normalized 3D distance...
     dpdf += interpol(yarr, 1.d3/pxarr, d) / n3d[ii] ; DIVIDE by distance!
  ENDFOR
  
  ;;==========================================================================
  ;; Maybe "fuzz" the DPDF based on 3D distance?  This would probably
  ;;   involve convolving the above-produced DPDF with a Gaussian in
  ;;   heliocentric distance, with the width of the kernel related to
  ;;   the 3D distance?  Any ideas?  Or do we just leave it this way?
  ;;   Who knows.
  
  
  ;; Add "fuzzing" code here...
  ;; Fuzzy Wuzzy was a bear
  ;; Fuzzy Wuzzy had no hair
  ;; Fuzzy Wuzzy wasn't fuzzy, was he?
  
  
  constrain = 1b                ; We produced a DPDF!
  
  ;; Normalize the DPDF to have unit total probability
  RETURN, dpdf / total(dpdf)
END
