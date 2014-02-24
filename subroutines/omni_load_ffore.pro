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
;
;-

PRO OMNI_LOAD_FFORE, cubefn
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON CUBE_BLOCK, ff_cube, ff_astr, ff_dist
  
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF n_elements(cubefn) EQ 0 THEN cubefn = local.mwffore
  
  ;; Load into the COMMON block!
  ff_cube = readfits(cubefn,hd)
  extast,hd,ff_astr
  ff_dist = findgen(sxpar(hd,'NAXIS3'))/sxpar(hd,'NAXIS3')*2.d4 + $
            sxpar(hd,'CRVAL3')
  
  RETURN
END
