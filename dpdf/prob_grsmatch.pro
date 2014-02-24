;+
; NAME:
;       PROB_GRSMATCH
;
; PURPOSE:
;       Creates a distance probability density function for a BGPS
;       source based on the GRS data set.  Using extracted sprectra
;       from the GRS data cubes using Erik's "on-off" method,
;       this routine calculates the kinematic distance pdf based on
;       the entire GRS spectrum.
;
; CATEGORY:
;       distance-omnibus
;
; CALLING SEQUENCE:
;       probability = PROB_GRSMATCH(struct, [,dvec = dvec, $
;                       spectrum = spectrum, bgps_ml = bgps_ml, $
;                       grs_dir = grs_dir, bgps_dir = bgps_dir, $
;                       map_dir = map_dir, obj_dir = obj_dir, $
;                       constrain = constrain])
;
; INPUTS:
;       struct  --  BGPS source structure (see READ_BGPS_CSV), which
;                 includes source longitude and latitude
;
; OPTIONAL INPUTS:
;       DVEC   -- Vector of distances (if not specified, will build
;                 one with values from galactic_params)
;
;       (The following may be needed if no GRS_SPEC structure exists):
;       BGPS_ML  -- BGPS map locations structure to use.  Defaults to
;                  'bgps_v102_map_locations.sav' in the "local" directory.
;       GRS_DIR  -- Directory containing GRS spectral line cubes.
;       BGPS_DIR -- Directory containing the BGPS data
;       MAP_DIR  -- Directory containing the BGPS IMAGE files
;       OBJ_DIR  -- Directory containing the BGPS object files
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       PROBABILITY -- probability vector normalized to unity.
;       CONSTRAIN   -- Does this routine provide a constraint on the
;                      BGPS source distance (1b) or return a uniform
;                      prior (0b)?
;
; OPTIONAL OUTPUTS:
;       SPECTRUM    -- Integrated masked spectrum used for probability
;                      calculation. 
;
; COMMON BLOCKS:
;       GRS_BLOCK -- Contains the GRS spectrum structure (created with
;                    generate_grs_spectra_local.pro).  If not present,
;                    routine will extract spectrum from GRS directly (slow!).
;
; MODIFICATION HISTORY:
;
;       Fri Jun 11 13:36:03 2010, erosolo <erosolo@>
;                 Documented
;
;       Thu Jul 22 11:14:30 2010, erosolo <erosolo@>
;		Updated to newest version.
;
;       Modified: 08/04/10, TPEB -- Modified to place the actual GRS
;                                   matching into a separate routine,
;                                   and to accept pre-extracted
;                                   spectra, if available.
;       Modified: 10/03/10, TPEB -- Further modified to place the
;                                   kinematic distance calculation
;                                   into a separate routine
;                                   (kdist_spectrum) because the code
;                                   was duplicated in prob_kdist.
;       Modified: 08/20/13, TPEB -- Following Edmonton2013, we have a
;                                   calibrated set of criteria for
;                                   applying this DPDF method.
;                                   Pulling this routine into OMNI_*
;                                   and implementing the new criteria.
;
;
;
;-

FUNCTION PROB_GRSMATCH, s, DVEC = dvec, CONSTRAIN=constrain, CONFFILE=cfile, $
                        SPECTRUM=spectrum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON GRS_BLOCK, grs, grsexist, v_std
  
  ;; Read in galactic-params & dpdf-params config files
  conf = omni_load_conf(cfile)
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF n_elements(dvec) THEN  d = dvec  ELSE $
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Initialize to uniform probability
  prob = fltarr(n_elements(d))+1d0/n_elements(d)  
  constrain = 0b
  
  ;; Quick checking of GRS information
  IF ~exist(grsexist) THEN grsexist=1b ; Assume for the moment it does...
  IF ~exist(v_std) THEN v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
  
  ;; Check if GRS file exists, but is not loaded
  IF grsexist + (size(grs,/TYPE) EQ 8) EQ 1 THEN BEGIN
     
     ;; Load configuration structures to get necessary info
     IF ~n_elements(conf) THEN conf = omni_load_conf(cfile)
     IF ~n_elements(ancil) THEN $
        ancil = omni_read_conffile('./conffiles/ancillary.conf')
     
     ;; GRS spectrum block filename
     grsfn = 'local/'+conf.survey+'_grs_spectra_r'+$
             string(ancil.grs_rad,format="(I0)")+'.sav'
     grsexist = FILE_TEST(grsfn,/READ)
     IF grsexist THEN restore,grsfn,/ver
  ENDIF
  
  ;;=================================================================
  ;; Get the GRS spectrum for this source, by hook or by crook
  CASE grsexist OF
     
     1: BEGIN                   ; Extract appropriate sepectrum from structure
        
        match = WHERE( grs.cnum EQ s.cnum, nmatch )
        IF nmatch NE 1 THEN RETURN, prob ; Return if no spectrum
        spectrum = grs[match].spectrum
        
     END
     0: BEGIN                   ; Structure does not exist, run OMNI_GRSMATCH
        
        spectrum = omni_grsmatch(s, v_std = v_std, onspec=onspec,$
                                 bdrspec=bdrspec, border=border, $
                                 bhdr=bhdr, R=r, STATUS=status)
        IF ~status THEN RETURN, prob ; Return if no spectrum
     END
  ENDCASE
  
  
  ;;=================================================================
  ;; Look at the spectrum, and decide if it's worth anything
  
  ;;=====
  ;; Maybe do the SavGol filtering here, if desired?
  
  omni_grs_maskspec, spectrum
  IF total(spectrum) EQ 0. THEN RETURN, prob ; No detection, return uniform
  
  ;; With masked spectrum in hand, compute kinematic distance!
  RETURN, omni_kdist_spectrum(s.glon, s.glat, v_std, spectrum, DVEC=dvec,$
                              CONSTRAIN=constrain)
  
END
