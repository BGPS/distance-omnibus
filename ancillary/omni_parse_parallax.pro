;+
; NAME:
;       OMNI_PARSE_PARALLAX
;
; PURPOSE:
;       Parse the parallax data table on the BeSSeL website.  Uses the
;       UNIX command WGET to retrieve the latest version of the table
;       in HTML format (presently the only format available) and parse
;       the fields into an IDL structure for use.
;
; CATEGORY:
;       distance-omnibus Subroutine
;
; CALLING SEQUENCE:
;       OMNI_PARSE_PARALLAX
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       NONE
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
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  12/07/13, TPEB -- Initial version.
;       Modified: 04/07/14, TPEB -- Modified to use published MRT from
;                                   Reid et al. (2014); 103 parallaxes
;
;-

PRO OMNI_PARSE_PARALLAX
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in the ancillary config file
  IF ~exist(ancil) THEN ancil = omni_read_conffile('./conffiles/ancillary.conf')
  
  t = read_mrt('./ancillary/Reid_2014_table1.mrt',COUNT=npx)
  
  ;; Initialize an array of structures
  bessel = replicate( {name:'',$
                       alias:'',$
                       l:0.d,$
                       b:0.d,$
                       px:0.d,$
                       e_px:0.d,$
                       pme:0.d,$
                       e_pme:0.d,$
                       pmn:0.d,$
                       e_pmn:0.d,$
                       vlsr:0.d,$
                       e_vlsr:0.d,$
                       arm:'',$
                       ref:''}, npx)
  
  ;; Load most everything directly across...
  bessel.name   = t.name
  bessel.alias  = t.alias
  bessel.px     = t.px
  bessel.e_px   = t.e_px
  bessel.pme    = t.pme
  bessel.e_pme  = t.e_pme
  bessel.pmn    = t.pmn
  bessel.e_pmn  = t.e_pmn
  bessel.vlsr   = t.vlsr
  bessel.e_vlsr = t.e_vlsr
  bessel.arm    = t.arm
  bessel.ref    = t.ref
  
  ;; Compute Galactic Coordinates
  RA  = 15.*(double(t.rah)+ double(t.ram)/60.d + double(t.ras)/3600.)
  DEC = double(t.de_+'1') * (double(t.ded) + double(t.dem)/60. + $
                             double(t.des)/3600.)
  
  euler,ra,dec,l,b,1
  bessel.l = l
  bessel.b = b
  
  save,bessel,filename='./ancillary/bessel_parallaxes.sav',/ver
END
