;+
; NAME:
;       OMNI_KINEMATIC_AVOIDANCE
;
; PURPOSE:
;       Set constrain.kdist = 0b (or constrain.grsmatch = 0b)
;       for objects in regions for which we don't trust
;       kinematic distances.  This includes the Galactic bar (as
;       outlined in Ellsworth-Bowers et al. 2013, ApJ, 770, 39), and
;       within 20 degrees of l=90.  Rejection is derived from the L-V
;       diagram.
;
;       *** IMPORTANT: This routine is currently only set up for the
;           BGPS in Quadrant 1 at l >= 7.5!
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       constr = OMNI_KINEMATIC_AVOIDANCE(glon, vlsr)
;
; INPUTS:
;       GLON -- Galactic longitude of the source [deg].
;       VLSR -- Observed V_LSR [km/s].
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       CONSTR -- Binary constraint based on whether or not the object
;                 lies in the 'rejection' zones of the L-V diagram. 
;                 1b = REJECT, 0b = OKAY
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  08/28/13, TPEB -- Initial version.  Then L-V regions
;                                   are pulled directly from
;                                   OMNI_GENERATE_EMAF.pro.
;       Modified: 09/03/13, TPEB -- Reversed sense of the output to
;                                   act as a flag for rejection.
;       Modified: 09/18/13, TPEB -- Added the +- 20 degr around the
;                                   Galactic anticenter for
;                                   rejection.  Changed the multiple
;                                   IF statements to a single CASE
;                                   statement.
;       Modified: 12/08/13, TPEB -- Cosmetic change to end of CASE
;                                   statement.  Also, force FLAG=0 for
;                                   sources w/o kinematic information
;                                   (i.e. vlsr = -1000), since the
;                                   kinematic distance DPDF routines
;                                   handle this already.
;       Modified: 02/11/14, TPEB -- Adjusted the l=90,270 KAZs to
;                                   reflect analysis of dv/dd.
;       Modified: 02/13/14, TPEB -- Name change OMNI_NOTTRUST_KDIST
;                                   --> OMNI_KINEMATIC_AVOIDANCE.
;
;-

FUNCTION OMNI_KINEMATIC_AVOIDANCE, glon, vlsr
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  CASE 1 OF
     
     ;; For NO KINEMATIC INFORMATION, return 0 -- not "untrusted",
     ;;   just non-existant.
     vlsr LE -300: RETURN,0b
     
     ;; Can't use objects w/in 21 deg of GC UNLESS part of
     ;;   Molecular Ring structure.  NOTE: This rejection is valid for
     ;;   the 1ST QUADRANT ONLY -- need to add rejection for 
     ;;   4TH QUADRANT when such need arises.
     glon LT 21.: BEGIN
        IF (vlsr GT (10.d/3.d*glon + 15.d)) || $
           (vlsr LT (20.d/9.d*glon - 50.d/3.d)) THEN RETURN,1b
     END
     
     ;; Near l=90, remove a certain slice of the Galaxy -- see
     ;;   analysis in Ellsworth-Bowers et al. (2014, ApJ, VVV, PPP).
     (glon GE 70) && (glon LE 100): $
        ;; VLSR value chosen to encompass all of the Cyg X region
        IF vlsr GE -15. THEN RETURN,1b 
     
     ;; Can't use objects w/in 20 deg of l=180 because small
     ;;   changes in velocity inflict large changes in heliocentric
     ;;   distace.
     abs(glon - 180.) LT 20.: RETURN,1b
     
     ;; Near l=270, remove a certain slice of the Galaxy -- see
     ;;   analysis in Ellsworth-Bowers et al. (2014, ApJ, VVV, PPP).
     (glon LE 290) && (glon GE 260): $
        ;; Choose value for VLSR based on conditions near the Carina tangent
        IF vlsr LE 10. THEN RETURN,1b
     
     ELSE: RETURN, 0b
     
  ENDCASE
END
