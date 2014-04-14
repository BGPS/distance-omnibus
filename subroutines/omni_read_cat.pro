;+
; NAME:
;       OMNI_READ_CAT
;
; PURPOSE:
;       Wrapper for the READ_MRT routine for the purpose of reading in
;       a survey catalog and making it conform to certain standards.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       s = OMNI_READ_CAT( [catfile] [,ncat][,fmt] )
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CATFILE -- Filename of the survey catalog, usually passed as
;                  conf.cat (from omni_read_conffile.pro).
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       S -- Standardized catalog structure
;
; OPTIONAL OUTPUTS:
;       NCAT -- Number of catalog items (elements in s)
;       FMT  -- Format string based on ncat
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  02/25/13, TPEB -- Initial version.
;       Modified: 02/26/13, TPEB -- Added FMT optional output.
;       Modified: 03/04/13, TPEB -- Updated documentation.
;       Modified: 06/03/13, TPEB -- Changed FMT output to handle
;                                   non-consecutive catalog numbers.
;       Modified: 11/06/13, TPEB -- Made CATFILE optional, assuming
;                                   default conf.cat is desired.
;
;-

FUNCTION OMNI_READ_CAT, catfile, ncat, fmt_out
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  IF ~exist(conf) THEN conf = omni_load_conf()
  IF n_elements(catfile) EQ 0 THEN catfile = conf.cat
  
  ;; Check that catfile can be read
  IF ~FILE_TEST(catfile,/read) THEN BEGIN
     message,'Error: Catalog file '+catfile+' does not exist!',/cont
     RETURN,{error:'ERROR: '+catfile+' does not exist'}
  ENDIF
  
  ;; Read in the survey catalog & count entries
  s = read_mrt(catfile)
  ncat = n_elements(s)
  
  ;; Check that the structure contains a few valid entries
  ;; Entries that MUST be present are: CNUM, NAME, GLON, GLAT
  bad = 0b
  IF ~TAG_EXIST(s,'CNUM') THEN bad = 1b
  IF ~TAG_EXIST(s,'NAME') THEN bad = 1b
  IF ~TAG_EXIST(s,'GLON') THEN bad = 1b
  IF ~TAG_EXIST(s,'GLAT') THEN bad = 1b
  
  IF bad THEN BEGIN
     message,'Error: Minimum necessary structure tags do not exist in '+$
             catfile+'  Required tags: CNUM, NAME, GLON, GLAT',/cont
     RETURN,{error:'ERROR: Minimum necessary structure tags do not exist.'}
  ENDIF
  
  ;; Create the format string based on values of CNUM
  q = '"'
  fmt = string(ceil(alog10(max(s.cnum)+1)),format="('(I0',I0,')')")
  
  ;; Set for optional output
  fmt_out = fmt
  
  RETURN,s
END
