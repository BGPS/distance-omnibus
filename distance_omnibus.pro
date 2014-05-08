;+
; NAME:
;       DISTANCE_OMNIBUS
;
; PURPOSE:
;       Master routine for the distance-omnibus code repository.
;       Running this routine SHOULD automatically read in all the
;       configuration files, create all necessary code-created local
;       files, and compute DPDFs for objects in the survey catalog.
;
; CATEGORY:
;       distance-omnibus Primary
;
; CALLING SEQUENCE:
;       distance_omnibus[,CNUM_LIST=cnum_list][,START=start][,REAR=rear] 
;                       [,/VERBOSE][,/PLOT][,/PS]
;                        
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       *Note: (CNUM_LIST) and (START, REAR) are mutually exclusive
;              options: if CNUM_LIST is supplied, then START & REAR
;              are ignored. 
;       CNUM_LIST     -- List of SURVEY catalog numbers for objects to
;                        calculate (as opposed to running the entire
;                        catalog).
;       START         -- First SURVEY catalog number to process
;                        [Default: #1]
;       REAR          -- Last SURVEY catalog number to process
;                        [Default: last entry in the catalog]
;
; KEYWORD PARAMETERS:
;       VERBOSE       -- Verbose output
;       PLOT          -- Plot each SURVEY source w/ individual and combined
;                        PDF to screen (or PostScript when used with /PS).
;       PS            -- Generate postscript files for the plots
;                        created with the \PLOT keyword [NOTE: only
;                        works in conjunction with \PLOT]
;
; OUTPUTS:
;       NONE
;
; OPTIONAL OUTPUTS:
;       NONE (See Keyword Parameters)
;
; COMMON BLOCKS:
;       OMNI_CONFIG    -- The set of configuration structures, read in
;                         from the config files in conffiles/
;       EMAF_BLOCK     -- Contains the EMAF structure required to tell if
;                         a particular source may have an EMAF DPDF
;                         computed.
;       VEL_BLOCK      -- Contains information about the dense gas
;                         velocity spectra.
;       PARALLAX_BLOCK -- Contains the BeSSeL maser parallax
;                         measurements.
;
; NOTES:
;       (From a conversation with Erik):
;       - A Master routine.  The routine should 
;       - Read in the original CSV file
;       - Search the code directory for all prob_* methods
;       - Run each SURVEY source through every prob_* method
;       - Evaluate (and weight) the joint probabilities to assess the distance
;       - This is currently handled by a rough routine called PROBEVAL
;         which is pretty simple and can be refined. 
;       - Write unique distances and uncertainties into the CSV file
;       - Export the CSV file.
;
; MODIFICATION HISTORY:
;       Created:  06/21/10, TPEB -- Initial version.
;       Modified: 07/09/10, TPEB -- Made csv_file an optional input
;                                   with default if not specified.
;       Modified: 02/01/11, TPEB -- Compatibility updates for the new
;                                   version of irdc_gauss.pro using the
;                                   full Peretto & Fuller (2009) IRDC
;                                   catalog, plus additional
;                                   documentation.
;       Modified: 07/13/11, TPEB -- Cleaned up the memory hog by
;                                   undefining the big variables at
;                                   the end of the routine.
;       Modified: 08/16/11, TPEB -- Routine now only saves IDL
;                                   structures if a CNUM_LIST is not
;                                   provided.
;       Modified: 08/29/11, TPEB -- Bug fix to make CNUM_LIST and
;                                   START/REAR keywords mutually
;                                   exclusive, with CNUM_LIST taking
;                                   precedence.
;       Modified: 09/12/11, TPEB -- Switching to the use of cgColor()
;                                   in the PROBCOLOR.pro calls, and
;                                   conformal changes.
;       Modified: 03/06/13, TPEB -- In the shift to OMNI_*.pro
;                                   code and generalized configuration
;                                   file input: made compatible with
;                                   the new framework (extensive
;                                   changes).
;       Modified: 03/08/13, TPEB -- Adjust KDAR flag if
;                                   CONSTRAIN[j].post = 0
;       Modified: 03/12/13, TPEB -- Added pvec.stat element DUSE to
;                                   signify the distance to use --
;                                   will be DML unless KDAR = 'T'
;                                   (then it will be DBAR).
;       Modified: 03/13/13, TPEB -- Set 'well-constrained' limit at
;                                   FW68 <= 2.3 kpc.
;       Modified: 03/14/13, TPEB -- Added testing line for when
;                                   routine pukes on NaNs.
;       Modified: 03/20/13, TPEB -- Added functionality for outputting
;                                   subsets of sources to FITS for
;                                   ease of use.  Updated
;                                   documentation.
;       Modified: 04/02/13, TPEB -- Fixed minor issue in error-bar
;                                   fitting, setting the return array
;                                   to zeros.
;       Modified: 06/12/13, TPEB -- Fixed minor typo bug related to
;                                   exporting DPDFs as a FITS table.
;       Modified: 08/01/13, TPEB -- Check that the catalog file listed
;                                   in the map_locations file matches
;                                   the catalog file in the current
;                                   SURVEY_INFO configuration file.
;       Modified: 08/20/13, TPEB -- Added new GRS 13CO matching DPDF.
;                                   This is based on work from the
;                                   Edmonton2013 working group.
;       Modified: 08/21/13, TPEB -- Force constraint.grsmatch = 0 if
;                                   constraint.kdist = 1 to favor the
;                                   use of the dense gas tracer, where
;                                   available.
;       Modified: 08/22/13, TPEB -- In the well-constrained
;                                   determination, allow for GRSMATCH
;                                   in addition to KDIST.
;       Modified: 08/28/13, TPEB -- Added call to new OMNI_NOTTRUST_KDIST
;                                   in the well-constrained checking
;                                   part of the routine to check
;                                   whether the source lies in the
;                                   'rejection' regions of the L-V
;                                   diagram, as defined in the EMAF
;                                   paper.
;       Modified: 09/03/13, TPEB -- Added Galactic coordinates to the
;                                   PVEC and CONSTRAIN structures for
;                                   more versatility.  Also, asjusted
;                                   the manner in which
;                                   OMNI_NOTTRUST_KDIST works.
;       Modified: 09/18/13, TPEB -- To avoid being assigned KDAR 'U',
;                                   source must EITHER have a
;                                   kinematic distance OR be
;                                   associated with a KNOWND region.
;                                   If a source would be given KDAR
;                                   'X', but KNOWND is set, then allow
;                                   non-'X' KDAR.
;       Modified: 10/23/13, TPEB -- Allow SURVEY with no kinematic
;                                   information, just don't try
;                                   to set constrain.post.
;       Modified: 10/24/13, TPEB -- Allow for proper functioning of
;                                   the routine in the absence of
;                                   either GRSMATCH or KNOWND DPDFs.
;       Modified: 11/13/13, TPEB -- Added HRDS prior.
;       Modified: 12/07/13, TPEB -- Made PARALLAX functional!
;       Modified: 12/08/13, TPEB -- Moved plotting diagnostic over to
;                                   new routine OMNI_PLOT_DPDFS.pro.
;                                   Also, documentation cleanup.  Also
;                                   also, streamlined the logic for
;                                   inclusion of DPDFs to be logically
;                                   consistent with what I want this
;                                   damn thing to do.
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;       Modified: 12/12/13, TPEB -- Made HRDS functional!
;       Modified: 02/13/14, TPEB -- Name change OMNI_NOTTRUST_KDIST
;                                   --> OMNI_KINEMATIC_AVOIDANCE.
;                                   Also, now check for KAZ GRS VLSR.
;       Modified: 04/11/14, TPEB -- Tweak 'tangent' designation to
;                                   account for miscreant sources.
;       Modified: 05/08/14, TPEB -- Add LOCAL.OUTPUT element to point
;                                   to the actual output directory.
;
;-

PRO DISTANCE_OMNIBUS, CONFFILE=cfile,CNUM_LIST=cnum_list, VERBOSE=verbose, $
                      START=start, REAR=rear, PLOT=plot, PS=ps
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON VEL_BLOCK, v, v_std
  
  
  ;;=================================================================
  ;; Parse input
  verbose = KEYWORD_SET(verbose)
  plot    = KEYWORD_SET(plot)
  ps      = KEYWORD_SET(ps)
  IF ps THEN cgLoadct,13
  
  ;; Read in all configuration files -- load up OMNI_CONFIG
  conf = omni_load_conf(cfile)
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF ~exist(mw) THEN $
     mw = omni_read_conffile('./conffiles/galactic_params.conf')
  
  ;; Create distance array
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE $
     d = dindgen(dpdfs.nbins)*dpdfs.binsize + dpdfs.binstart
  ;;=================================================================
  
  
  
  ;;=================================================================
  ;; Check which DPDFs to run
  ntags = n_tags(dpdfs.dpdf)
  dvals = intarr(ntags)
  FOR tag=0,ntags-1 DO dvals[tag] = dpdfs.dpdf.(tag)
  dpdfi = where(dvals, nrun)
  IF ~nrun THEN BEGIN
     message,'Error: No DPDFs selected to run in dpdf_params.conf!  '+$
             'Exiting.',/cont
     RETURN
  ENDIF
  
  ;; DTAGS contains only the DPDF tags in the order specified
  dtags = (tag_names(dpdfs.dpdf))[dpdfi[sort(dvals[dpdfi])]]
  ;;=================================================================
  
  
  
  ;;=================================================================
  ;; Begin loading required information, according to specified DTAGS
  
  
  ;; Check that the current catalog version is the same as that used
  ;;   for OMNI_ASSOC_CATALOG.  If not, delete extant IDL files.
  mlfn = './local/'+conf.survey+'_map_locations.sav'
  IF FILE_TEST(mlfn,/READ) THEN restore,mlfn,VERBOSE=verbose
  undefine,survey
  IF ~n_elements(survey_info) || survey_info.cat NE conf.cat THEN BEGIN
     fn = file_search('./local/'+conf.survey+'_*.sav', COUNT=nfn)
     IF nfn THEN file_delete,fn,/ALLOW_NONEXISTENT
  ENDIF
  
  ;; MAP_LOCATIONS save file -- Check existance and run
  mlfn = './local/'+conf.survey+'_map_locations.sav'
  IF ~FILE_TEST(mlfn,/READ) THEN omni_assoc_catalog
  
  ;; Read in SURVEY structure
  s   = omni_read_cat(conf.cat,ncat,fmt)
  fmt2 = string(ceil(alog10(ncat+1)),format="('I',I0)")
  
  ;; Velocity - Kinematic Distances
  IF fix(total(dtags EQ 'KDIST')) THEN BEGIN
     vfn = './local/'+conf.survey+'_velocities.sav'
     IF ~FILE_TEST(vfn,/READ) THEN omni_import_velocity
     restore,vfn,VERBOSE=verbose
     do_kdist = 1b
  ENDIF ELSE do_kdist = 0b
  
  ;; Velocity - GRS 13CO Spectra
  IF fix(total(dtags EQ 'GRSMATCH')) THEN BEGIN
     IF ~exist(ancil) THEN $
        ancil = omni_read_conffile('./conffiles/ancillary.conf')
     gfn = 'local/'+conf.survey+'_grs_spectra_r'+$
           string(ancil.grs_rad,format="(I0)")+'.sav'
     IF ~FILE_TEST(gfn,/READ) THEN omni_grs_spectra_local
     
     COMMON GRS_BLOCK, grs, grsexist, v_standard
     restore,gfn,VERBOSE=verbose
     grsexist = 1b
     do_grs = 1b
  ENDIF ELSE do_grs = 0b
  
  ;; EMAF
  IF fix(total(dtags EQ 'EMAF')) THEN BEGIN
     COMMON EMAF_BLOCK, emaf
     efn = './local/'+conf.survey+'_emaf.sav'
     IF ~FILE_TEST(efn,/READ) THEN omni_generate_emaf
     restore,efn,VERBOSE=verbose
  ENDIF
  
  ;; MASER PARALLAX
  IF fix(total(dtags EQ 'PARALLAX')) THEN BEGIN
     COMMON PARALLAX_BLOCK, bessel, adist
     bfn = './ancillary/bessel_parallaxes.sav'
     IF ~FILE_TEST(bfn,/READ) THEN omni_parse_parallax
     restore,bfn,/ver
  ENDIF
  
  ;; HRDS -- HII REGIONS
  IF fix(total(dtags EQ 'HRDS')) THEN BEGIN
     COMMON HRDS_BLOCK, hrds, hdist
     IF ~exist(hrds) THEN hrds = read_mrt('./ancillary/HRDS_catalog.mrt')
  ENDIF
  
  ;; HISA -- NOT CURRENTLY FUNCTIONAL!!!
  IF fix(total(dtags EQ 'HISA')) THEN BEGIN
     message,'Warning: Requested HISA as one of the DPDFs.  '+$
             'Not yet supported/functional.  '+$
             'Removing HISA from the list of DPDFs to run.',/cont
     dtags = dtags[MISSING(['HISA'],dtags)]
     ;; COMMON HI_BLOCK, hi
     ;; IF n_elements(hi_spectrum) EQ 0 THEN $
     ;;    hi_spectrum = './local/bgps_hi_spectra.sav'
     ;; IF file_test(hi_spectrum,/READ) THEN restore,hi_spectrum,/ver ELSE $
     ;;    hi = ''
  ENDIF
  
  ;; IRDC -- NOT CURRENTLY FUNCTIONAL!!!
  IF fix(total(dtags EQ 'IRDC')) THEN BEGIN
     message,'Warning: Requested IRDC as one of the DPDFs.  '+$
             'Not yet supported/functional.  '+$
             'Removing IRDC from the list of DPDFs to run.',/cont
     dtags = dtags[MISSING(['IRDC'],dtags)]
     ;; COMMON IRDC_BLOCK
  ENDIF
  
  ;; NIREX -- NOT CURRENTLY FUNCTIONAL!!!
  IF fix(total(dtags EQ 'NIREX')) THEN BEGIN
     message,'Warning: Requested NIREX as one of the DPDFs.  '+$
             'Not yet supported/functional.  '+$
             'Removing NIREX from the list of DPDFs to run.',/cont
     dtags = dtags[MISSING(['NIREX'],dtags)]
     ;; COMMON NIREX_BLOCK
  ENDIF
  
  ;; MST -- NOT CURRENTLY FUNCTIONAL!!!
  IF fix(total(dtags EQ 'MST')) THEN BEGIN
     message,'Warning: Requested MST as one of the DPDFs.  '+$
             'Not yet supported/functional.  '+$
             'Removing MST from the list of DPDFs to run.',/cont
     dtags = dtags[MISSING(['MST'],dtags)]
     ;; COMMON MST_BLOCK
  ENDIF
  ;;=================================================================
  
  
  
  ;;=================================================================
  ;; Check that the desired DPDF routines exist.
  prob_list = 'prob_'+strlowcase(dtags)
  extant = FILE_TEST('./dpdf/'+prob_list+'.pro',/READ)
  iyes = where(extant,n_prob)
  IF ~n_prob THEN BEGIN
     message,'Error: None of the requested DPDFs exist!  Exiting.',/cont
     RETURN
  ENDIF
  ino = where(~extant,nno)
  IF nno THEN $
     FOR kk=0,nno-1 DO $
        message,'Warning: Requested '+dtags[ino[kk]]+' as one of the DPDFs.  '+$
                'Not yet supported/functional.  '+$
                'Removing '+dtags[ino[kk]]+' from the list of DPDFs to run.',$
                /cont
  dtags     = dtags[iyes]
  prob_list = prob_list[iyes]
  ;;=================================================================
  
  
  
  ;;=================================================================
  IF verbose THEN print,n_prob,prob_list
  ;; Build IDL command string for each of the specified DPDF methods
  ;;    and the array of structures to hold the pvec arrays
  pvec      = {cnum:0L}
  constrain = {cnum:0L}
  
  FOR i=0L, n_prob-1 DO BEGIN
     ;; Create command for this DPDF method
     stub = STRMID( prob_list[i], 5 )              
     prob_list[i] = 'pvec[j].' + stub + ' = ' + prob_list[i] + $
                    '( s[j], DVEC=dvec, CONSTRAIN=constr )'
     ;; Add this DPDF to the outout structures
     pvec      = CREATE_STRUCT(pvec,      stub, dblarr(dpdfs.nbins))
     constrain = CREATE_STRUCT(constrain, stub, 0b)
  ENDFOR
  
  ;; Add pvec structure element for posterior DPDF & Coordinates!
  pvec      = CREATE_STRUCT(pvec,'POST',dblarr(dpdfs.nbins),$
                            'GLON',0.d,'GLAT',0.d)
  constrain = CREATE_STRUCT(constrain,'POST',0b,$
                            'GLON',0.d,'GLAT',0.d)
  
  ;; Add substructure containing distance estimate stats
  str = {dml:[0.,0.,0.],$
         dbar:[0.,0.],$
         duse:[0.,0.,0.],$
         pml:0.,$
         fw68:0.,$
         dtan:0.d,$
         kdar:'U'}              ; Initialize to 'Unconstrained'
  pvec = CREATE_STRUCT(pvec, 'nbins',dpdfs.nbins,'binsize',dpdfs.binsize,$
                       'binstart',dpdfs.binstart,'STAT',str)
  
  IF verbose THEN BEGIN
     print,prob_list
     help,pvec,/str
     help,pvec.stat,/str
     help,constrain,/str
  ENDIF
  ;;=================================================================
  
  
  
  ;;=================================================================
  ;; If CNUM_LIST supplied, then only do the big loop for those objects
  IF n_elements(cnum_list) THEN BEGIN
     ncat = n_elements(cnum_list) 
     ind = WHERE_ARRAY(cnum_list, s.cnum)
     s = s[ind]                 ; SURVEY structure
     v = v[ind]                 ; VELOCITY structure
     start = 0L
     rear = ncat-1
  ENDIF ELSE BEGIN
     ;; Else, look for START & REAR keywords and adjust accordingly
     start = n_elements(start) ? long(start) : 0L
     rear  = n_elements(rear)  ? long(rear)  : ncat-1
  ENDELSE  
  
  ;; Replicate pvec, one for each SURVEY source
  pvec      = REPLICATE(pvec,      ncat)
  constrain = REPLICATE(constrain, ncat)
  
  ;; Load the CNUM into the new structures
  pvec.cnum      = s.cnum
  constrain.cnum = s.cnum
  ;; Load Galactic coordinates into the new structures
  pvec.glon      = s.glon
  pvec.glat      = s.glat
  constrain.glon = s.glon
  constrain.glat = s.glat
  
  ;;=================================================================
  
  
  ;;***********************************************************
  ;; Before getting started, get index in prob_list containing
  ;;    GRSMATCH
  pgi = where(strupcase(strmid(prob_list,8,8)) EQ 'GRSMATCH',npgi)
  ;;***********************************************************
  
  
  ;;=================================================================
  ;; Plotting Stuff
  IF plot AND ~ps THEN window,2
  
  ;; Before we beging computing the DPDFs for each object, set a flag
  ;;   for whether a survey has kinematic information at all...
  has_kin = fix(total(strmatch(dtags,'KDIST'))) || $
            fix(total(strmatch(dtags,'GRSMATCH')))
  
  
  ;; Pause 10 seconds for station identification...
  message,'Pausing 10 seconds for station identification...',/inf
  wait,10
  
  
  ;; Loop for running each SURVEY source through each specified 
  ;;   prob_* method. 
  FOR j=start, rear DO BEGIN
     
     message,string(s[j].cnum,format=$
                    "('Computing DPDFs for "+conf.survey+" object #',"+fmt2+$
                    ",' ...')"),/inf
     
     ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
     ;; Set flag for rejection of kinematic information based on
     ;;   location in the L-V diagram
     CASE 1 OF
        v[j].vlsr GE -500.: vlsr = v[j].vlsr                   
        where([1,2,5,6] EQ v[j].grs.flag) NE -1: vlsr = v[j].grs.vlsr 
        ELSE: vlsr = -1000.
     ENDCASE
     reject_kd = OMNI_KINEMATIC_AVOIDANCE(v[j].l,vlsr)
     ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
     
     ;;=================================================================
     ;; Set posterior probability to unity, then multiply DPDFs
     ;;    together! 
     pvec[j].post = d*0.d + 1.d
     
     FOR i=0L, n_prob-1 DO BEGIN
        errcode = Execute( prob_list[i] ) ; Run the probability!
        constrain[j].(i+1) = constr ; The (i+1) skips over the .CNUM element
        
        ;;**********************************************************
        ;; Check that we only apply the GRS 13CO if the dense gas
        ;;   KDIST has constr = 0
        IF npgi THEN IF i EQ pgi && constrain[j].kdist THEN BEGIN
           constrain[j].(i+1) = 0b
           CONTINUE
        ENDIF
        ;;**********************************************************
        
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; For kinematic distance DPDFs (i.e. KDIST, GRSMATCH), apply
        ;; the OMNI_KINEMATIC_AVOIDANCE check here, and only multiply in
        ;; the kinematic information if acceptable...
        IF reject_kd && (strmatch(prob_list[i],'*kdist*') || $
                         strmatch(prob_list[i],'*grsmatch*')) THEN BEGIN
           constrain[j].(i+1) = 0b ; Set constraint = 0b
           CONTINUE
        ENDIF
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        
        ;; Otherwise, multiply in the computed DPDF if constrain = 1b
        IF constrain[j].(i+1) THEN pvec[j].post *= pvec[j].(i+1)
     ENDFOR
     
     ;; Normalize to unit integral probability
     pvec[j].post /= TOTAL(pvec[j].post)
     
     
     ;;=================================================================
     ;;=================================================================
     ;; Evaluation of the posterior DPDF
     ;;   Criteria are laid out in Ellsworth-Bowers et al. (2013, ApJ,
     ;;   770, 39).
     
     dtan = mw.r0 * cos(s[j].glon*!dtor) / cos(s[j].glat*!dtor)
     pvec[j].stat.dtan = dtan
     
     ;; Compute D_ML and associated error bars
     pvec[j].stat.dml = OMNI_COMPUTE_EBARS( pvec[j].post, d )
     
     ;; Compute FW68
     pvec[j].stat.fw68 = pvec[j].stat.dml[1] + pvec[j].stat.dml[2]
     
     ;; If INNER GALAXY, find regions on either side of DTAN
     IF (s[j].glon GT -90. && s[j].glon LT 90.) || $
        (s[j].glon GT 270.) THEN BEGIN
        
        nind = where(d LE dtan, nnear)
        find = where(d GT dtan, nfar)
        ;; Compute P_ML
        pvec[j].stat.pml = total(pvec[j].post[nind]) > total(pvec[j].post[find])
        ;; Make N/F/T determination -- Tangent if <~ 1 kpc of dtan
        pvec[j].stat.kdar = pvec[j].stat.dml[0] LE dtan ? 'N' : 'F'
        IF abs(pvec[j].stat.dml[0] - dtan) LE 1.05d3 THEN $
           pvec[j].stat.kdar = 'T'
        
     ENDIF ELSE BEGIN
        ;; OUTER GALAXY sources, P_ML = 1., KDAR = 'O'
        pvec[j].stat.pml = 1.d
        pvec[j].stat.kdar = 'O'
     ENDELSE
     
     
     ;; Compute moment distances (dbar)
     pvec[j].stat.dbar[0] = total(pvec[j].post * d)
     pvec[j].stat.dbar[1] = sqrt( total(pvec[j].post * d * d) - $
                                  pvec[j].stat.dbar[0] * pvec[j].stat.dbar[0] )
     
     
     ;; Fill in DUSE elements
     pvec[j].stat.duse = (pvec[j].stat.kdar EQ 'T') ? $
                         [pvec[j].stat.dbar[0],$
                          pvec[j].stat.dbar[1],$
                          pvec[j].stat.dbar[1]] : $
                         pvec[j].stat.dml
     ;;=================================================================
     ;;=================================================================
     
     
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Check to see if survey kas kinematic information.  If not,
     ;;   then skip this step.  This check is to allow the code to not
     ;;   puke on SURVEYS that have no kinematic information.
     IF has_kin THEN BEGIN
        
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Set POSTERIOR CONSTRAINT and KDAR flags based on statistics
        ;;   values and other relevant information.
        
        ;; To have a "well-constrained" distance estimate, a SURVEY
        ;;   source must have a constraint from at least ONE of the
        ;;   following:   [KDIST, GRSMATCH, KNOWND, PARALLAX]
        constrain_arg = constrain[j].kdist
        IF conf.usegrs THEN $
           constrain_arg = constrain_arg || constrain[j].grsmatch 
        IF dpdfs.dpdf.knownd NE 0 THEN $
           constrain_arg = constrain_arg || constrain[j].knownd
        IF dpdfs.dpdf.parallax NE 0 THEN $
           constrain_arg = constrain_arg || constrain[j].parallax
        
        ;; Set CONSTRAIN[j].POST=1b IF above criteria are met, AND the
        ;;   FW68 is less than 2.3 kpc *OR* D_ML is w/in ~1 kpc of DTAN
        IF constrain_arg && $
           (pvec[j].stat.fw68 LE 2.3d3 || $
            abs(pvec[j].stat.dml[0] - pvec[j].stat.dtan) LE 1.05d3) THEN $
               constrain[j].post = 1b ELSE $
                  pvec[j].stat.kdar = 'U' ; If CONSTRAIN[j].post = 0
        
        ;;********************************************************************
        ;; Check for whether this source is in the Galactic bar --
        ;;   don't serve it any more alcohol -- also, reject
        ;;   sources near l=90 and l=180, unless **KNOWND OR PARALLAX**
        ;;   are set.  Essentially, looking at the KINEMATIC-ONLY
        ;;   sources that passed the above test, and setting KDAR='X'
        ;;   if they are in a rejected region.  However, this code
        ;;   should NEVER be run, since we already set kinematic
        ;;   constraints to zero above...
        
        has_kid = 0b            ; Kinematic-Independent Distance Measurement
        IF dpdfs.dpdf.knownd NE 0 THEN $
           has_kid = has_kid || constrain[j].knownd
        IF dpdfs.dpdf.parallax  NE 0 THEN $
           has_kid = has_kid || constrain[j].parallax
        
        IF reject_kd && ~has_kid THEN BEGIN
           ;; Set the CONSTRAIN.POST and KDAR correctly if adjusted above.
           constrain[j].post = 0b
           pvec[j].stat.kdar = 'X' ; X for eXcluded from sample based on L-V
        ENDIF
        
        ;;********************************************************************
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
     ENDIF
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
     ;;===== BEGIN PLOTTING JUNK =====
     IF plot THEN BEGIN
        IF ps THEN BEGIN
           filename = string(s[j].cnum,format="('"+local.output+conf.survey+$
                             "_prob',"+fmt2+",'.eps')")
           IF verbose THEN print,filename
           myps,filename
        ENDIF
        
        ;; Use the fancy new plotting routine!
        omni_plot_dpdfs,pvec[j]
        
        ;; Add distance information to the plot
        al_legend,box=0,position=[0.70*!x.crange[1],0.6*!y.crange[1]],$
                  [string(pvec[j].stat.dml[0]/1.d3,format=$
                          "('Dist = ',F5.2,' kpc')")]
        
        ;; Various program control related to waiting...
        IF ~ps THEN wait,0.05
        IF (constrain[j].kdist || constrain[j].grsmatch) && ~ps THEN $
           wait,2.0
        IF ps THEN myps,/done
     ENDIF
     ;;===== END PLOTTING JUNK =====
     
  ENDFOR                        ; End loop over sources
  
  
  ;; Save the PVEC and CONSTRAIN structures to disk in both IDL SAVE
  ;;   and FITS format for later use, but only if the entire catalog
  ;;   was computed (i.e. CNUM_LIST NOT SET).
  IF n_elements(cnum_list) EQ 0 THEN BEGIN
     save,pvec,constrain,filename='./local/'+conf.survey+'_pvec.sav',/ver
     IF dpdfs.fits THEN omni_export_fits,'./local/'+conf.survey+'_pvec.sav'
  ENDIF
  
  ;; Clean up the memory, dude
  undefine,pvec,v,grs,hi
  
END
