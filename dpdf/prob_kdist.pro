;+
; NAME: 
;       PROB_KDIST
;
; PURPOSE:
;       Calculate probability of observing the given velocity as a
;       function of distance of the source.  Uses Reid et al. (2009)
;       recommended method. 
;
; CATEGORY:
;       distance-omnibus DPDF Generation Routine
;
; CALLING SEQUENCE:
;       probability = PROB_KDIST(struct [,DVEC=dvec]
;                     [,CONSTRAIN=constrain]) 
;
; INPUTS:
;       STRUCT -- SURVEY source structure (see OMNI_READ_CATALOG.pro),
;                 which includes source longitude and latitude.
;
; OPTIONAL INPUTS:
;       DVEC    -- Vector of distances (if not specified, will build
;                  one with values from dpdf_params.conf)
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
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
;       VEL_BLOCK   -- Contains the velocity structure being used.
;
; MODIFICATION HISTORY:
;
;       Tue Oct 20 10:36:51 2009, Erik Rosolowsky <erosolo@A302357>
;		Documented
;
;       Modified: 06/03/10, TPEB -- Defined standard structure input
;       Modified: 10/03/10, TPEB -- Moved functionality to
;                                   kdist_spectrum.pro, since the code
;                                   is identical to that in
;                                   prob_grsmatch.
;       Modified: 04/27/11. TPEB -- Code & Documentation Cleanup.
;       Modified: 02/28/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: made compatible with the
;                                   new framework.
;       Modified: 03/05/13, TPEB -- Added check for VEL_BLOCK,
;                                   allowing the routine to read in
;                                   the structure on-the-fly if not
;                                   previously loaded.
;       Modified: 03/20/13, TPEB -- Added CONFFILE optional input for
;                                   conformity with other routines.
;       Modified: 08/20/13, TPEB -- Minor code cleanup.
;       Modified: 12/17/13, TPEB -- Added a means for squashing the
;                                   near probability for objects
;                                   beyond the Solar Circle.
;       Modified: 01/09/14, TPEB -- Fixed bug related to VEL_BLOCK
;                                   usage.
;
;-

;;==================================================================;;
;; Subroutine for squashing near probability for objects beyond the ;;
;;   Solar Circle                                                   ;;
;;==================================================================;;
PRO PROB_KDIST_SQUASH, dpdf, l, b
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Load Necessary Configuration Files, and parse
  IF ~exist(mw) THEN $
     mw = omni_load_conffile('./conffiles/galactic_params.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_load_conffile('./conffiles/dpdf_params.conf')
  
  ;; Set up standard distance vector
  d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Find where d is less than the tangent point
  dind = where(d LE mw.R0*cos(l*!dtor)/cos(b*!dtor),nd)
  IF nd NE 0 THEN dpdf[dind] = 0. ; Squash!
  
  RETURN
END


;;===============;;
;; Main Function ;;
;;===============;;
FUNCTION PROB_KDIST, s, DVEC=dvec, CONSTRAIN=constrain, CONFFILE=cfile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON VEL_BLOCK, v, v_std
  
  constrain = 0b
  l = s.glon
  b = s.glat
  
  ;; Read in velocity structure (& config file), if necessary
  IF ~exist(v) THEN BEGIN
     conf = omni_load_conf(cfile)
     vfn = './local/'+conf.survey+'_velocities.sav'
     IF ~FILE_TEST(vfn,/READ) THEN BEGIN
        message,'Warning:  No velocity structure save file present.  '+$
                'Running OMNI_IMPORT_VELOCITY...',/inf
        omni_import_velocity
     ENDIF
     restore,vfn,/ver
     v_std = findgen(conf.nvbin) * conf.deltav + conf.vstart
  ENDIF
  
  ;; Pick off the element of v corresponding to the object contained
  ;;   in the structure s.
  this   = WHERE(v.cnum EQ s.cnum, nind)
  IF ~nind THEN RETURN, fltarr(n_elements(d))+1d0/n_elements(d) ; Error check
  v_spec = omni_vspec( [1.,v[this].vlsr,v[this].lw], v_std )
  
  dpdf = omni_kdist_spectrum(l, b, v_std, v_spec, DVEC=dvec, $
                             CONSTRAIN=constrain)
  
  ;; Check for objects in Quadrants I and IV beyond the Solar Circle &
  ;;   squash residual 'NEAR' probability.
  CASE 1 OF
     
     l GT 0. && l LT 90.: $     ; Quadrant I
        IF v[this].vlsr LT -5. THEN prob_kdist_squash, dpdf, l, b
     
     l LT 0. || l GT 270.: $    ; Quadrant IV
        IF v[this].vlsr GT +5. THEN prob_kdist_squash, dpdf, l, b
     
     ELSE:                      ; Go out for a beer!
  ENDCASE
  
  RETURN, dpdf
END
