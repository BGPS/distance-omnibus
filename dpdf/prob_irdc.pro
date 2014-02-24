;+
; NAME: 
;       PROB_IRDC
;
; PURPOSE: 
;       Return the probability vector as a function of distance if an
;       IRDC is along the line of sight, with flexible use of
;       association algorithms.
;
; CALLING SEQUENCE:
;       probability = PROB_IRDC(struct [,dvec = dvec][,ALGORITHM=algorithm] )
;
; INPUTS:
;       STRUCT -- BGPS source structure (see READ_BGPS_CSV), which
;                 includes source longitude and latitude, and a flag
;                 which will be set for presence of IRDC
;
; OPTIONAL INPUTS:
;       DVEC        -- Vector of distances (if not specified, will build
;                      one with values from galactic_params)
;       ALGORITHM   -- Select which association algorithm to use 
;                      0 = IRDC_LOOKUP, 1 = IRDC_GAUSS, 2 = IRDC_MORPH
;                      [Default = IRDC_GAUSS]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       PROBABILITY -- propbability vector as a function of distance
;       CONSTRAIN   -- Does this routine provide a constraint on the
;                      BGPS source distance (1b) or return a uniform
;                      prior (0b)?
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
;       Modified: 12/12/13, TPEB -- Changed rotuine WEIGHT_PROB -->
;                                   OMNI_WEIGHT_DPDF to reflect new
;                                   naming convention.
;
;-

FUNCTION PROB_IRDC, s, DVEC = dvec, CONSTRAIN = constrain, $
                    ALGORITHM = algorithm, _EXTRA=_extra, $
                    PDFDC=pdfdc, DIFF=diff, DOF=dof
  
  ;; Check on the association algorithm to use
  IF n_elements(algorithm) EQ 0 THEN algorithm = 1b ELSE $
     algorithm = byte(algorithm)
  
  ;; Parse structure into useable variables
  l = s.glon_peak
  b = s.glat_peak
  
  ;; Get galactic params
  defsysv, '!MW', exists = exists
  IF NOT exists THEN galactic_params 
  R0 = !mw.r0
  V0 = !mw.v0
  IF n_elements(dvec) NE 0 THEN  d = dvec  ELSE $
     d = dindgen(!MW.NBINS)*!MW.BINSIZE + !MW.BINSTART
  
  ;; Set internal routine params
  width = 200.0                 ; Width of the rolloff (in pc) for the prob_vec
  
  
  ;; Initialize to uniform probability
  prob = fltarr(n_elements(d))+1d0/n_elements(d)  
  constrain = 0b
  
  ;; Check to see if we have an IRDC
  CASE algorithm OF  
     0: irdc = irdc_lookup(l, b)
     1: irdc = irdc_gauss(s)
     2: BEGIN
        prob = irdc_morph(s, d, PDFDC=pdfdc, DIFF=diff, DOF=dof, _EXTRA=_extra)
        irdc = 0b
     ENDCASE
     ELSE: message,'ALGORITHM value not recognized.  Exiting...'
  ENDCASE  
  
  ;; Only change probability & flag if we do have an IRDC.
  IF irdc GT 0. THEN BEGIN
     
     ;; Tangent distance
     tandist = R0*cos(l*!dtor)
     
     ;; Cut probability on this side of the tangent point; normalize
     prob = 1-erf((d-tandist)/width)
     prob /= total(prob)
     constrain = 1b
     
     IF irdc LT 1 THEN $
        prob = omni_weight_dpdf( prob, irdc )
     
     ;; Set IRDC flag in the structure
     s.assoc_features = set_assoc_flag(s.assoc_features, 'irdc_alg')
     
  ENDIF
  
  RETURN, prob
END
