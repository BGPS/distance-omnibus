;+
; NAME:
;       OMNI_GRS_MASKSPEC
;
; PURPOSE:
;       Takes the 'rind extracted' On-Off 13CO spectrum around a
;       SURVEY object, and masks it to reveal just the primary peak.
;       Conditional criteria for multi-peaked spectra are included.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       OMNI_GRS_MASKSPEC, spectrum
;
; INPUTS:
;       SPECTRUM -- The input full 13CO 'rind extracted' spectrum.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       SPECTRUM -- The masked 13CO 'rind extracted' spectrum.
;
; OPTIONAL OUTPUTS:
;       FLAG -- Specifies what the spectrum looks like:
;               0: Non-detection
;               1: Single-peak detection
;               2: Multiple peaks, but able to distinguish a primary
;               3: Multiple peaks, unable to distinguish (returns blank)
;               4: Landlocked ONSPEC only (not assigned here, but
;                  aligned w/ OMNI_GRSMATCH)
;
; COMMON BLOCKS:
;       OMNI_CONFIG    -- The set of configuration structures, read in
;                         from the config files in conffiles/
;       GRS_BLOCK      -- Contains the GRS spectrum structure (created
;                         with generate_grs_spectra_local.pro).
;
; MODIFICATION HISTORY:
;
;       Created:  08/22/13, TPEB -- Initial version, with code
;                                   extracted largely from
;                                   prob_grsmatch.pro.
;       Modified: 08/26/13, TPEB -- Added FLAG optional output to
;                                   specify what the spectrum looks
;                                   like.
;       Modified: 11/19/13, TPEB -- Did slight modification for faster
;                                   runtime -- no other outwardly
;                                   visible changes.
;       Modified: 11/12/13, TPEB -- Fixed bug introduced with above
;                                   performance-enhancing drug.
;       Modified: 09/17/14, TPEB -- Fix issue when multiple peaks are
;                                   NOT separated by a valley dipping
;                                   below ancil.grs_ta.  Once regions
;                                   above ancil.grs_ta are identified,
;                                   recompute the peak indices using
;                                   the greater of ancil.grs_ta or the
;                                   (peak TA* / ancil.grs_ratio).
;       Modified: 09/21/14, TPEB -- Correct fix for the "Big Island"
;                                   problem, which was attempted in
;                                   the 09/17/14 fix.  Now, the
;                                   routine identifies islands above
;                                   the THRESHOLD value, then analyzes
;                                   each island for multiple summits
;                                   (a la the Big Island of Hawaii).
;                                   In this way, if one island has
;                                   multiple summits, they will be
;                                   analyzed individually using the
;                                   criteria for multiple peaks in the
;                                   spectrum.  Add Check_Math() calls
;                                   to silence underflow errors.
;
;-

PRO OMNI_GRS_MASKSPEC, spectrum, flag
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON GRS_BLOCK, grs, grsexist, v_std
  
  flag = 0b                     ; Non-detection
  
  ;; Read in survey-info config file, if necessary
  conf = omni_load_conf(cfile)
  IF ~exist(ancil) THEN $
     ancil = omni_read_conffile('./conffiles/ancillary.conf')
  
  ;; Check for v_std... create from configuration file
  IF ~exist(v_std) THEN $
     v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
  
  ;; Check the existance and format of SPECTRUM
  IF ~exist(spectrum) || n_elements(spectrum) NE conf.nvbin THEN BEGIN
     message,'SPECTRUM either does not exist or does not conform to '+$
             'sirvey-info.conf.  Returning blank array.',/cont
     spectrum *= 0.
     RETURN
  ENDIF
  
  ;;=================================================================
  ;; Look at the spectrum, and decide if it's worth anything
  
  ;; Find contiguous regions above SPECIFIED VALUE in the On-Off spectrum
  si = where(spectrum GE ancil.grs_ta, nsi)
  
  ;; IF no detection at all, then return blank
  IF nsi EQ 0 THEN BEGIN
     spectrum *= 0.
     RETURN
  ENDIF
  
  ;; Compute the STEPS between indices above THRESHOLD; if there
  ;;   are gaps, this indicates multiple "islands" above the
  ;;   THRESHOLD sea.
  si   = [si,n_elements(spectrum)] ; Needed for proper operation
  step = si[1:*] - si[0:*]         ; Steps between indices above THRESHOLD
  
  ;; Compute the number of segments based on STEP
  ij = [where(step NE 1),n_elements(step)-1] ; Find the jumps
  ij = ij[uniq(ij,sort(ij))]                 ; Make unique
  nseg = n_elements(ij)                      ; Number of segments
  
  ;; Find brightest peak
  sti = 0                       ; Starting index
  tpk = max(spectrum[si])       ; Absolute peak TA* in On-Off spectrum
  tas = list(!null)             ; LIST to hold TA* for each peak
  kk  = -1                      ; Segment counter to include multi-multi's
  
  void = Check_Math()           ; Clear out Math Errors
  
  FOR jj=0, nseg-1 DO BEGIN     ; Loop over segments
     si2 = si[sti:ij[jj]]       ; Array elements for this peak
     sti = ij[jj] + 1           ; Set up for start of next peak
     
     ;; Check size of this peak... if less than 2, then skip to next peak
     IF n_elements(si2) LT 2 THEN CONTINUE
     kk++                       ; Increment KK counter if not skipped
     
     ;; Check to see if this peak is a "Big Island" type island,
     ;;    meaning there are multiple summits in a single island
     ;;    rising above the THRESHOLD sea.  Compute the
     ;;    "derivative" of the spectrum & look for discontinuous
     ;;    regions for the positive "slope".  (Quotes used
     ;;    because we're not dividing by dx here... only
     ;;    using dy).
     spec_seg = spectrum[si2]                 ; Extract just this peak
     dy       = spec_seg[1:*] - spec_seg[0:*] ; Compute dy
     seg_sti  = 0                             ; Index within this peak
     seg_inds = lindgen(n_elements(si2))      ; Indices within this peak
     v_seg    = v_std[si2]                    ; Velocities for this peak
     
     ;; Find elements of - slope, add last element for proper operation
     downslope  = [where(dy LT 0.),n_elements(spec_seg)-1]
     ;; Steps between indices for regions of - slope.
     sl_step = downslope[1:*] - downslope[0:*]
     
     void = Check_Math()        ; Clear out Math Errors
     
     ;; Compute the number and location(s) of "saddle points",
     ;;    where n_saddle = # jumps in downslope
     ;;    == First, compute the number of summits based on SL_STEP
     kl = [where(sl_step NE 1),n_elements(downslope)-1] ; Find the jumps
     kl = kl[where(kl GE 0)]       ; Remove any "-1" that may exist.
     kl = kl[uniq(kl,sort(kl))]    ; Make unique
     n_summit = n_elements(kl)     ; N summits
     
     
     ;; Indices of Saddle Locations.  Note that if n_saddle == 0,
     ;;   then kl = -1, which places saddles = last element of
     ;;   segment, which is just where we want it! 
     saddles = downslope[kl]
     
     ;; Loop through summits
     FOR ll=0, n_summit-1 DO BEGIN
        
        ;; Augment KK, if necessary.
        IF ll GE 1 THEN kk++
        
        ;; Array elements for this summit out of this island 
        seg_si2 = seg_inds[seg_sti:saddles[ll]]
        tas.add, max(spec_seg[seg_si2]), kk ; TA* for this summit
        seg_sti = saddles[ll] + 1           ; Set up for start of next summit
        
        void = Check_Math()     ; Clear out Math Errors
        
        IF tas[kk] LT tpk THEN CONTINUE ; If not brightest peak, skip to next
        
        ;; Fit a gaussian to this peak
        est = [tas[kk],v_seg[median(seg_si2)],2.]
        yft = mpfitpeak(v_std, spectrum, A, NTERMS=3, ESTIMATES=est)
        vel = A[1]
        sig = A[2]
     ENDFOR
     
  ENDFOR
  
  void = Check_Math()           ; Clear out Math Errors
  
  ;; Convert the LIST to an ARRAY for analysis.  First, remove the
  ;;   '!null' in the last element.
  tas.remove
  tas = tas.ToArray()
  
  ;; Error checking
  IF tpk EQ 0 THEN BEGIN
     spectrum *= 0.
     RETURN
  ENDIF
  
  ;; Check the ratio of TA* between primary and secondary
  ;;   peak -- If LT SPECIFIED VALUE, then return uniform
  ;;           DPDF, since the peaks are indistinguishable. 
  nseg = n_elements(tas)        ; Redefine nseg, in the event of "Big Islands"
  rat = tpk / tas
  
  ;; SET FLAGS APPROPRIATELY: 0,1,2,3
  CASE 1 OF
     
     nseg EQ 1: flag = 1b       ; Single Peak
     
     nseg GT 1: BEGIN           ; Multiple Peaks
        
        IF min(rat[where(rat GT 1., nri)]) LT ancil.grs_ratio THEN BEGIN
           spectrum *= 0.       ; Null out spectrum
           flag = 3b            ; Multiple velocities -- unable to distinguish
           RETURN
        ENDIF
        
        flag = 2b               ; Multiple velocities -- able to distinguish
        
     END
     
     nseg EQ 0: BEGIN           ; Error catching -- should never run
        message,'Warning: No segments found at the end of the routine.  '+$
                'This may signify major issues.  Please see a qualified '+$
                'developer or just take several shots of rum.',/cont
        spectrum *= 0.
        RETURN
     END
     
     ELSE: $                   ; More error catching -- REALLY should never run
        message,'ERROR: "Danger, Will Robinson!"  Drop everything and '+$
                'figure out just what the hell is going on here!!!  '+$
                'Variable nseg LT 0.'
  ENDCASE                       ; End of FLAG-SET section
  
  ;; Mask out the portion of SPECTRUM beyond 3 sigma from this
  ;;   peak to remove 'noise' at other velocities.
  spectrum *= (abs( v_std - vel ) LE 3.d * sig)
  
  
  RETURN
  
END
