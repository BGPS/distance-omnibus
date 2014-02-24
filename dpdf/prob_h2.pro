;+
; NAME: 
;       PROB_H2
;
; PURPOSE: 
;       Return probability vector for cloud distance as a function of
;       longitude and latitude.  This assumes that the sources track
;       the Wolfire et al. (2003) H2 distribution.  
;
; CATEGORY:
;       distance-omnibus DPDF Generation Routine
;
; CALLING SEQUENCE:
;       probability = PROB_H2(struct [,DVEC=dvec]
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
;       Tue Oct 20 09:29:44 2009, Erik Rosolowsky <erosolo@A302357>
;		Documented.
;
;       Modified: 06/03/10, TPEB -- Defined standard structure input
;
;       Sun Jun 6 22:03:34 2010, erosolo <erosolo@>
;		Changed l/b refs to peak values.
;
;       Modified: 09/07/10, TPEB -- Refelcting change in routine name
;                                   h2_density.pro 
;       Modified: 05/23/11, TPEB -- Moved calculation of (R,Z) vectors
;                                   to the new routine lbd2rz.pro,
;                                   which takes into account
;                                   inaccuracies in the definition of
;                                   Galactic coordinates.
;       Modified: 08/16/11, TPEB -- Corrected typo in creation of the
;                                   uniform probability vector for
;                                   return if source is outside the
;                                   reasonable range for use with this
;                                   method.  NOTE: this typo did not
;                                   affect overall probability
;                                   calculations, since the product
;                                   probability is normalized to unit
;                                   total.
;       Modified: 09/15/11, TPEB -- Moved the GALACTIC_PARAMS call
;                                   earlier to ensure !MW is defined
;                                   before returning uniform probability.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: made compatible with the
;                                   new framework.
;       Modified: 03/06/13, TPEB -- Name change to H2 from LONLAT, to
;                                   make it easier to remember what it
;                                   does.
;       Modified: 12/12/13, TPEB -- Changed rotuine WEIGHT_PROB -->
;                                   OMNI_WEIGHT_DPDF to reflect new
;                                   naming convention.
;       Modified: 01/31/14, TPEB -- Small GLON updates to keep
;                                   function working in Quadrant IV.
;
;-

FUNCTION PROB_H2, s, DVEC=dvec, CONSTRAIN=constrain
  
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
  uniform = fltarr(dpdfs.nbins) + 1.d0/(dpdfs.nbins)
  
  ;; Check if LOS is valid for using this method...
  min_r  = 0.2                  ; Miminum legal galactocentric radius / R0
  ell_lim = ASIN(min_r) / !dtor ; degrees
  
  ;; Use a CASE statement to reject sources!
  CASE 1 OF
     (l LE ell_lim) AND (l GE -ell_lim): RETURN, uniform ; Near GC
     (l GE 360. - ell_lim):              RETURN, uniform ; Near GC
     (l GT 70.) AND (l LT 290.):         RETURN, uniform ; Outer Galaxy
     (l LT -70.):                        RETURN, uniform ; Outer Galaxy
     ELSE: 
  ENDCASE
  
  ;; Calculate (R,Z) for each of the distances, given (l,b)
  omni_lbd2rz, l, b, d, r, z
  
  ;; Get density at that R,Z -- then normalize the probability
  rho = h2_density(r, z)
  prob = rho/total(rho)
  
  ;; De-weight according to fractional error between Bronfmann data & model
  value = 1. - 0.1              ; De-wieght to 90%
  constrain = 1b
  RETURN,  omni_weight_dpdf( prob, value )
  
END
