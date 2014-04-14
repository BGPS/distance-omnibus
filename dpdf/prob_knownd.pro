;+
; NAME: 
;       PROB_KNOWND
;
; PURPOSE: 
;       Return probability vector for cloud distance based on known
;       regions of star formation whose distances have been
;       well-determined by parallax.  This is really only valid for
;       regions in the outer Galaxy, since confusion is vastly
;       minimized out in the Galactic hinterlands (i.e. the ass end of
;       space).
;
; CATEGORY:
;       distance-omnibus DPDF Generation Routine
;
; CALLING SEQUENCE:
;       probability = PROB_KNOWND(struct [,DVEC=dvec]
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
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  09/18/13, TPEB -- Initial version (cut-and-pasted
;                                   from PROB_PARALLAX.pro).  Set up as a
;                                   kludge for the Gem OB1 region.
;       Modified: 09/19/13, TPEB -- Artificially added a spread in the
;                                   distances by convolving with a
;                                   gaussian sigma = 0.2 kpc.
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;       Modified: 02/14/14, TPEB -- Add check for DPDF_PX, if that
;                                   exists, return uniform DPDF here.
;       Modified: 02/19/14, TPEB -- Updated GemOB1 code to incorporate
;                                   separate distance estimates for
;                                   regions on either side of l=191.4
;                                   degrees.
;
;-

FUNCTION PROB_KNOWND, s, DVEC = dvec, CONSTRAIN = constrain
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in galactic-params & dpdf-params config files
  IF ~exist(mw) THEN mw = omni_read_conffile('./conffiles/galactic_params.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE $
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Parse structure into useable variables
  l = s.glon
  b = s.glat
  constrain = 0b
  
  
  ;; Set this up as a CASE statement to choose regions of interest
  CASE 1 OF
     
     ;;===============
     ;; GEM OB1
     abs(l - 190.5) LE 3.5 AND abs(b - 0.25) LE 1. : BEGIN
        
        ;; Check if this source has a valis DPDF_PX from the expanded
        ;;   set of BeSSeL parallaxes published in Reid et al. (2014,
        ;;   arXiv:1401.5377).  If so, return uniform prior here.
        px_dpdf = prob_parallax(s, CONSTRAIN=cp)
        IF cp THEN RETURN, fltarr(dpdfs.nbins) + 1.d0/(dpdfs.nbins)
        
        IF l LE 191.4 THEN BEGIN
           ;; Parallax information for S252 from Reid et al. (2009,ApJ,693,397) 
           pi  = 0.476d-3       ; [arcsec] parallax 
           spi = 0.006d-3       ; [arcsec] uncertainty in parallax
        ENDIF ELSE BEGIN
           ;; Parallax information for S255 from Rygl et al. (2010,A&A,511,2)
           pi  = 0.628d-3       ; [arcsec] parallax 
           spi = 0.027d-3       ; [arcsec] uncertainty in parallax
        ENDELSE
        
        ;; Define a parallax array appropriate, and populate Gaussian
        piarr = dindgen(1001)/1000.d * 1d-3 ; parallax array from 0 to 1 mas
        paral = gauss2(piarr,[1,pi,spi])
        
     END
     
     ;;===============
     ;; Others?
     
     
     ;; Return uniform probability if object is not in one of these regions
     ELSE: RETURN, fltarr(dpdfs.nbins) + 1.d0/(dpdfs.nbins)
  ENDCASE
  
  ;; Interpolate from parallax grid to distance grid
  prob = interpol(paral,1.d/piarr,d) > 0.
  
  ;; For the fun of it, convolve with Gaussian sigma = 0.2 kpc
  dc = d[0:long(floor(n_elements(d)/4.-1))]
  prob = convol(prob,gauss2([dc-max(dc),dc],[1,0.d3,0.2d3]),/edge_trun,/nan) >0.
  prob /= total(prob)
  
  constrain = 1b
  RETURN, prob
  
END
