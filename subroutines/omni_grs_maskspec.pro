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
  ;; Check for the absolute peak TA* in these regions.
  tpk = max(spectrum[si])       ; Absolute peak TA* in On-Off spectrum
  ;; Recompute contiguous regions based on the threshold peak-to-peak
  ;;    ratio (if criteria is larger than ancil.grs_ta). (9/17/14)
  si = where(spectrum GE (ancil.grs_ta > (tpk/ancil.grs_ratio)), nsi)
  
  IF nsi GT 1 THEN BEGIN        ; Only consider detections
     
     step = si[1:*] - si[0:*]   ; Steps between indices above THRESHOLD
     
     CASE (max(step) EQ 1) OF
        
        1: BEGIN                ; Single Peak
           
           nseg = 1             ; Number of segments
           flag = 1b            ; Flag for single peak
           
           ;; Fit a gaussian to the peak
           est = [max(spectrum), v_std[median(si)], 2.]
           yft = mpfitpeak(v_std, spectrum, A, NTERMS=3, ESTIMATES=est)
           
           ;; Mask out the portion of SPECTRUM beyond 3 sigma from the
           ;;   peak to remove 'noise' at other velocities.
           spectrum *= (abs( v_std - A[1] ) LE 3.d * A[2])
           
        END
        0: BEGIN                ; Multiple Peaks
           
           si   = [si,n_elements(spectrum)] ; Needed for proper operation
           step = si[1:*] - si[0:*]         ; Recompute step
           
           ij = [where(step NE 1),n_elements(step)-1] ; Find the jumps
           ij = ij[uniq(ij,sort(ij))]                 ; Make unique
           nseg = n_elements(ij)                      ; Number of segments
           
           ;; Find brightest peak
           sti = 0                 ; Starting index
           tpk = max(spectrum[si]) ; Absolute peak TA* in On-Off spectrum
           tas = fltarr(nseg)      ; Array to hold TA* for each peak
           
           FOR jj=0, nseg-1 DO BEGIN ; Loop over segments
              
              si2 = si[sti:ij[jj]]         ; Array elements for this peak
              tas[jj] = max(spectrum[si2]) ; TA* for this peak
              sti = ij[jj] + 1             ; Set up for start of next peak
              
              IF tas[jj] LT tpk THEN CONTINUE ; If not brightest peak, skip
              
              ;; Fit a gaussian to this peak
              est = [tas[jj],v_std[median(si2)],2.]
              yft = mpfitpeak(v_std, spectrum, A, NTERMS=3, ESTIMATES=est)
              tpk = tas[jj]
              vel = A[1]
              sig = A[2]
           ENDFOR
           
           ;; Error checking
           IF tpk EQ 0 THEN BEGIN
              spectrum *= 0.
              RETURN
           ENDIF
           
           ;; Check the ratio of TA* between primary and secondary
           ;;   peak -- If LT SPECIFIED VALUE, then return uniform
           ;;           DPDF, since the peaks are indistinguishable. 
           
           rat = tpk / tas
           IF min(rat[where(rat GT 1., nri)]) LT ancil.grs_ratio THEN BEGIN
              spectrum *= 0.    ; Null out spectrum
              flag = 3b         ; Multiple velocities -- unable to distinguish
              RETURN
           ENDIF
           
           ;; Mask out the portion of SPECTRUM beyond 3 sigma from this
           ;;   peak to remove 'noise' at other velocities.
           spectrum *= (abs( v_std - vel ) LE 3.d * sig)
           flag = 2b            ; Multiple velocities -- able to distinguish
           
        END
     ENDCASE
     
  ENDIF ELSE spectrum *= 0.     ; No detection, return blank
  
  RETURN
  
END
