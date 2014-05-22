;+
; NAME:
;       OMNI_LOAD_FFORE
;
; PURPOSE:
;       Load the FFORE data cube (l,b,d) into its COMMON block for use
;       with the IRDC morphological matching code.
;
; CATEGORY:
;       distance-omnibus subroutine
;
; CALLING SEQUENCE:
;       OMNI_LOAD_FFORE [,cubefn]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CUBEFN -- Filename of the (l,b,d) cube to read in.  
;                 [Default: ./local/MW_model_ffore.fits]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG   -- The set of configuration structures, read in
;                        from the config files in conffiles/
;       CUBE_BLOCK    -- Block holding the FFORE data cube (l,b,d)
;                        needed to generate the DPDF.
;
; MODIFICATION HISTORY:
;
;       Created:  09/27/12, TPEB -- Initial version.
;       Modified: 03/04/13, TPEB -- Changed name of cube file, and in
;                                   the shift to OMNI_*.pro code, name
;                                   change.
;       Modified: 11/01/13, TPEB -- Removed hardwire location of MW
;                                   model, replacing with element from
;                                   local configuration file.
;       Modified: 05/21/14, TPEB -- Added check for the existance of
;                                   the MW model... if not extant,
;                                   spew long error message and stop
;                                   execution.  Also, add COMPILE_OPT
;                                   statements.
;
;-

PRO OMNI_LOAD_FFORE, cubefn
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON CUBE_BLOCK, ff_cube, ff_astr, ff_dist
  
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF ~n_elements(cubefn) THEN cubefn = local.mwffore
  
  ;; Check for the existance of cubefn -- if not, stop execution!
  IF ~FILE_TEST(cubefn,/READ) THEN BEGIN
     message,'ERROR: The expected foreground fraction model does not exist '+$
             'at the specified location in conffiles/local_layout.conf!  '+$
             'This model is required for computation of the EMAF-based prior '+$
             'DPDF.  The file MW_model_ffore.fits may be downloaded from the '+$
             'BGPS archive at IPAC.  To run distance_omnibus without this '+$
             'model (and EMAF-based DPDF), deselect EMAF in '+$
             'conffiles/dpdf_params.conf.'
  ENDIF
  
  ;; Load into the COMMON block!
  ff_cube = readfits(cubefn,hd)
  extast,hd,ff_astr
  ff_dist = findgen(sxpar(hd,'NAXIS3'))/sxpar(hd,'NAXIS3')*2.d4 + $
            sxpar(hd,'CRVAL3')
  
  RETURN
END
