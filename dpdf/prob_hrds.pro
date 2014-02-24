;+
; NAME: 
;       PROB_HRDS
;
; PURPOSE: 
;       Return probability vector for cloud distance based on a
;       lookup-table of HRDS (HII Regions) KDA Resolutions
;
; CATEGORY:
;       distance-omnibus DPDF Generation Routine
;
; CALLING SEQUENCE:
;       probability = PROB_HRDS(struct [,DVEC=dvec]
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
;       N_HRDS      -- Number of HRDS HII regions for which this
;                      object falls inside the PPV association
;                      volume.
;       KDAR        -- Bitwise flag for KDAR(s) of contributing HRDS
;                      HII regions.  1 = N, 2 = F, 4 = T
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;       HRDS_BLOCK  -- Contains the HRDS catalog table.
;       VEL_BLOCK   -- Contains information about the dense gas
;                      velocity spectra.
;
; MODIFICATION HISTORY:
;
;       Created:  11/13/13, TPEB -- Initial version (cut & paste from
;                                   prob_parallax.pro)
;       Modified: 12/11/13, TPEB -- Utilize the criteria developed for
;                                   PROB_PARALLAX.pro to associate
;                                   SURVEY objects with HRDS HII
;                                   regions and apply a de-weighted
;                                   step function (if N or F) or
;                                   de-weighted Gaussian (if T) prior
;                                   DPDF based on the HRDS findings.
;       Modified: 02/13/14, TPEB -- Added KDAR optional output, to
;                                   analyze the properties of multiple
;                                   input HII regions, and make sure
;                                   we also allow vlsr association
;                                   with GRS 13CO.
;
;-

FUNCTION PROB_HRDS, s, DVEC = dvec, CONSTRAIN = constrain, N_HRDS=n_hrds, $
                    KDAR=kdar
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON HRDS_BLOCK, hrds, hdist
  COMMON VEL_BLOCK, v, v_std
  
  ;; Initialize to no KDAR
  kdar = 0b
  
  ;; Read in galactic-params & dpdf-params config files
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  IF ~exist(ancil) THEN ancil = omni_read_conffile('./conffiles/ancillary.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE $
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Check to see that HRDS is loaded & compute HDIST for common block
  IF ~exist(hrds) THEN hrds = read_mrt('./ancillary/HRDS_catalog.mrt')
  IF ~exist(hdist) THEN BEGIN
     dist  = hrds.dsun*1.d3               ; [pc] Distance to the HII region
     hdist = ancil.ppv_dis / dist / !dtor ; [degr] Angular association distance
  ENDIF
  
  ;; Parse structure into useable variables
  vel = v[where(v.cnum EQ s.cnum)]
  l   = s.glon
  b   = s.glat
  constrain = 0b
  
  ;;===================================================================
  ;; Check the (l,b) association based on ADIST and VLSR association
  gcirc, 2, l, b, hrds.glon, hrds.glat, dis
  
  ;; Choose VLSR from dense gas or GRS 13CO
  CASE 1 OF
     vel.vlsr GE -500.: vlsr = vel.vlsr                          ; OK dg vlsr
     where([1,2,5,6] EQ vel.grs.flag) NE -1: vlsr = vel.grs.vlsr ; OK GRS flag
     ELSE: vlsr = -1000.
  ENDCASE
  dv  = abs(hrds.vlsr - vlsr)
  
  ind = where( (dis/3600. LE hdist) AND (dv LE ancil.ppv_dv), n_hrds )
  
  ;; If nothing, then return uniform prior
  IF n_hrds EQ 0 THEN RETURN, fltarr(dpdfs.nbins) + 1.d0/(dpdfs.nbins)
  
  ;; Otherwise, cull arrays and compute...
  hrd = hrds[ind]               ; HRDS structure
  dis = dis[ind]                ; Angular distance [arcsec] 
  dv  = dv[ind]                 ; Velocity separation [km/s]
  adb = hdist[ind]              ; Angular association radius [deg]
  
  ;; Compute the normalized 3D distance...  sqrt(ang^2 + vel^2)
  n3d = sqrt((dis*dis/adb/adb/3600./3600.) + (dv*dv/ancil.ppv_dv/ancil.ppv_dv))
  
  ;;===========================================
  ;; Build the DPDF
  
  dpdf    = d * 0.d              ; DPDF array
  tandist = mw.R0 * cos(l*!dtor) ; Tangent distance
  width   = mw.step_width        ; [pc] Width of the rolloff of the ERF()
  
  ;; Loop through the number of HRDS regions associated with this
  ;;   catalog object. 
  FOR ii=0,n_hrds-1 DO BEGIN
     
     ;; Determine KDAR, and construct the proper DPDF -- namely
     ;;   construct the step function (or gaussian), and add it bassed
     ;;   on the normalized 3D distance.
     CASE 1 OF
        
        ;; NEAR
        strmatch(hrd[ii].kdar,'N*'): BEGIN
           dpdf += (1.d - erf( (d-tandist) / width)) / n3d[ii]
           kdar = kdar OR 1
        END
           
        ;; FAR
        strmatch(hrd[ii].kdar,'F*'): BEGIN
           dpdf += (1.d + erf( (d-tandist) / width)) / n3d[ii]
           kdar = kdar OR 2
        END
        
        ;; TANGENT -- The amplitude is comensurate with the erf()'s above.
        strmatch(hrd[ii].kdar,'T*'): BEGIN
           dpdf += gauss2(d,[2.,tandist,width])/n3d[ii]
           kdar = kdar OR 4
        END
        
        ELSE:                   ; If no value, then do nothing.
     ENDCASE
     
  ENDFOR
  
  ;; Check that the DPDF constains something, else return uniform prior.
  IF total(dpdf) EQ 0 THEN RETURN, fltarr(dpdfs.nbins) + 1.d0/(dpdfs.nbins)
  
  ;; De-weight by a constant value...
  dpdf = omni_weight_dpdf(dpdf, 0.9) ; Same weight as PROB_H2!
  constrain = 1b                     ; We produced a DPDF!
  
  ;; Normalize the DPDF to have unit total probability
  RETURN, dpdf / total(dpdf)
  
END
