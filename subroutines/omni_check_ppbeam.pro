;+
; NAME:
;       OMNI_CHECK_PPBEAM
;
; PURPOSE:
;       Checks the FITS keyword PPBEAM in the survey map images to
;       ensure it is the correct value.  This routine is necessary for
;       BGPS_V1 data, but may be relevant for other surveys.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       res = OMNI_CHECK_PPBEAM( hdr )
;
; INPUTS:
;       HDR -- The FITS header from a survey map.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       RES -- 0 for header is OKAY, and 1 for header was CORRECTED
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  07/19/11, TPEB -- Initial Version.
;       Modified: 02/26/13, TPEB -- Shifted to OMNI_*.pro structure,
;                                   including the input of conffile
;                                   data.
;
;-

FUNCTION OMNI_CHECK_PPBEAM, hdr, conf
  
  COMPILE_OPT IDL2, HIDDEN, LOGICAL_PREDICATE
  ON_ERROR,2
  
  IF abs(sxpar(hdr,'PPBEAM') - conf.ppbeam) LE 0.1 THEN RETURN,0b ELSE BEGIN
     sxaddpar, hdr, 'PPBEAM', conf.ppbeam, FORMAT="G19.12"
     message,'Correcting the PPBEAM for this file.',/inf
     RETURN,1b
  ENDELSE
  
END
