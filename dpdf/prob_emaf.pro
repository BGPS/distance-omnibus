;+
; NAME: 
;       PROB_EMAF
;
; PURPOSE: 
;       Calculate the DPDF for a source following the EMAF method
;       presented in Ellsworth-Bowers et al. (2013, ApJ, 770, 39).
;
; CATEGORY:
;       distance-omnibus DPDF Generation Routine
;
; CALLING SEQUENCE:
;       probability = PROB_EMAF(struct [,DVEC=dvec]
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
;       EMAF_BLOCK  -- Contains the EMAF structure required to tell if
;                      a particular source may have an EMAF DPDF
;                      computed.
;
; MODIFICATION HISTORY:
;
;       Tue Oct 20 10:41:30 2009, Erik Rosolowsky <erosolo@A302357>
;		Written
;       Modified: 06/03/10, TPEB -- Defined standard structure input
;                                   and flag setting for probability routines.
;       Sun Jun 6 22:06:05 2010, erosolo <erosolo@>
;		Amended l/b to use GLON_PEAK and GLAT_PEAK
;       Modified: 02/01/11, TPEB -- Added input ALGORITHM to select
;                                   between the various association
;                                   algorithms [Default = IRDC_GAUSS].
;       Modified: 09/08/11, TPEB -- Added ability to use new IRDC
;                                   matching routine IRDC_MORPH()
;                                   for creating IRDC distance
;                                   association proiors.
;       Modified: 07/02/12, TPEB -- Modified input to include _EXTRA
;                                   keyword for passing to the
;                                   irdc_morph.pro routine.
;       Modified: 02/28/13, TPEB -- Name change from IRDC -> EMAF to
;                                   reflect the paper referenced
;                                   above.  In the shift to OMNI_*.pro
;                                   code and generalized configuration
;                                   file input: name change and made
;                                   compatible with the new
;                                   framework.
;       Modified: 03/20/13, TPEB -- Added CONFFILE optional input for
;                                   conformity with other routines.
;       Modified: 08/20/13, TPEB -- Minor code cleanup.
;       Modified: 11/01/13, TPEB -- Added conffile means for
;                                   requesting /MAKE_PS in
;                                   omni_emaf_morph.pro.
;
;-

FUNCTION PROB_EMAF, s, DVEC=dvec, CONSTRAIN=constrain, CONFFILE=cfile, $
                    ALGORITHM=algorithm, PDFDC=pdfdc, DIFF=diff, DOF=dof, $
                    _EXTRA=_extra
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON EMAF_BLOCK, emaf
  
  ;; Read in dpdf-params & local-layout config files
  conf = omni_load_conf(cfile)
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF n_elements(dvec) THEN  d = dvec  ELSE $
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  
  ;; Initialize to uniform probability
  prob = fltarr(n_elements(d))+1d0/n_elements(d)  
  constrain = 0b
  
  ;; Check on EMAF structure
  IF ~exist(emaf) THEN BEGIN
     efn = './local/'+conf.survey+'_emaf.sav'
     IF ~FILE_TEST(efn,/READ) THEN omni_generate_emaf
     restore,efn,/ver
  ENDIF
  
  ;; Check if the survey has LABEL maps
  IF ~conf.haslabel THEN BEGIN
     message,'Warning: '+conf.survey+' does not have label maps.  '+$
             'PROB_EMAF cannot be computed!',/cont
     RETURN,prob
  ENDIF  
  
  ;; Check that this object is contained in EMAF, else return uniform
  ;;    probability
  IF ~fix(total( emaf.cnum EQ s.cnum )) THEN RETURN,prob
  
  prob = omni_emaf_morph(s, d, DIFF=diff, DOF=dof, MAKE_PS=local.makeemafps, $
                         _EXTRA=_extra)
  
  ;; Normalize and set constrain
  prob /= TOTAL(prob)
  constrain = 1b
  
  RETURN, prob
END
