;+
; NAME:
;       OMNI_READ_CONFFILE_DPDF
;
; PURPOSE:
;       Parses the DPDF-type configuration file.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE_DPDF( lun, conffile, lnum )
;
; INPUTS:
;       LUN      -- The logical unit number for the configuration file.
;       CONFFILE -- Name of the configuration file, used only for
;                   error display.
;       LNUM     -- Line number, used only for error display.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       CONF -- Structure containing the fields in the configuration
;               file.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  02/19/13, TPEB -- Initial version.
;       Modified: 02/21/13, TPEB -- Split out parsing of individual
;                                   config types into subfunctions.
;       Modified: 02/28/13, TPEB -- Added DPDF type parsing.
;       Modified: 03/01/13, TPEB -- Placed which/when DPDF run
;                                   information into substructure for
;                                   DPDF type config.
;       Modified: 03/20/13, TPEB -- Added parameters related to FITS
;                                   bintable output for DPDFs to the
;                                   DPDF type, and added the new MASS
;                                   type configuration file.
;       Modified: 03/21/13, TPEB -- Added BREAK statements instead of
;                                   RETURN statements in error-
;                                   catching code to ensure we close &
;                                   free file pointers.  Added
;                                   parameters to MASS type for
;                                   construction of the mass
;                                   function.  Placed MASS type DPDF
;                                   constraints into substructure.
;       Modified: 09/03/13, TPEB -- Updating DPDF type for use with
;                                   the GRSMATCH kinematic distance
;                                   likelihood DPDF.
;       Modified: 09/18/13, TPEB -- Added KNOWND prior DPDF.
;       Modified: 11/13/13, TPEB -- Added HRDS prior DPDF.
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;       Modified: 12/13/13, TPEB -- Added SAVE_ALL to DPDF for
;                                   specifying to save ALL objects to
;                                   the FITS bintable.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;
;-

;;=========================================================================
;; Function for the parsing of dpdf-params configuration files.
FUNCTION OMNI_READ_CONFFILE_DPDF, lun, conffile, lnum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Create the configuration structure for TYPE dpdf
  conf = {nbins:0,$
          binsize:0.d,$
          binstart:0.d,$
          dpdf: {kdist:0,$
                 h2:0,$
                 emaf:0,$
                 nirex:0,$
                 hisa:0,$
                 irdc:0,$
                 parallax:0,$
                 grsmatch:0,$
                 knownd:0,$
                 hrds:0,$
                 mst:0},$
          fits:0b,$
          save_all:0b,$
          fitsout: {vlsr:0b,$
                    h2:0b,$
                    emaf:0b,$
                    nirex:0b,$
                    hisa:0b,$
                    irdc:0b,$
                    parallax:0b,$
                    knownd:0b,$
                    hrds:0b,$
                    mst:0b},$
          ta_cont:0.d,$
          ta_hi:0.d,$
          error:''}
  
  ;; Continue reading the configuration file
  WHILE ~EOF(lun) DO BEGIN
     
     key = OMNI_READ_CONFFILE_READLINE(lun, conffile, lnum, val, nword, skip)
     IF skip THEN CONTINUE
     
     CASE key OF
        ;; DPDF Constriction
        'nbins':      conf.nbins      = (nword EQ 1) ? 0L  : long(val)
        'binsize':    conf.binsize    = (nword EQ 1) ? 0.d : double(val)
        'binstart':   conf.binstart   = (nword EQ 1) ? 0.d : double(val)
        
        ;; Order to run DPDFs
        'kdist':    conf.dpdf.kdist    = (nword EQ 1) ? 0L : long(val)
        'h2':       conf.dpdf.h2       = (nword EQ 1) ? 0L : long(val)
        'emaf':     conf.dpdf.emaf     = (nword EQ 1) ? 0L : long(val)
        'nirex':    conf.dpdf.nirex    = (nword EQ 1) ? 0L : long(val)
        'hisa':     conf.dpdf.hisa     = (nword EQ 1) ? 0L : long(val)
        'irdc':     conf.dpdf.irdc     = (nword EQ 1) ? 0L : long(val)
        'parallax': conf.dpdf.parallax = (nword EQ 1) ? 0L : long(val)
        'grs':      conf.dpdf.grsmatch = (nword EQ 1) ? 0L : long(val)
        'mst':      conf.dpdf.mst      = (nword EQ 1) ? 0L : long(val)
        'knownd':   conf.dpdf.knownd   = (nword EQ 1) ? 0L : long(val)
        'hrds':     conf.dpdf.hrds     = (nword EQ 1) ? 0L : long(val)
        
        'fits':     conf.fits     = (nword EQ 1) ? 0b : long(val) < 1
        'save_all': conf.save_all = (nword EQ 1) ? 0b : long(val) < 1
        ;; DPDFs required for FITS bintable output
        'has_vlsr':    conf.fitsout.vlsr     = (nword EQ 1) ? 0b : long(val) < 1
        'has_h2':      conf.fitsout.h2       = (nword EQ 1) ? 0b : long(val) < 1
        'has_emaf':    conf.fitsout.emaf     = (nword EQ 1) ? 0b : long(val) < 1
        'has_nirex':   conf.fitsout.nirex    = (nword EQ 1) ? 0b : long(val) < 1
        'has_hisa':    conf.fitsout.hisa     = (nword EQ 1) ? 0b : long(val) < 1
        'has_irdc':    conf.fitsout.irdc     = (nword EQ 1) ? 0b : long(val) < 1
        'has_parallax':conf.fitsout.parallax = (nword EQ 1) ? 0b : long(val) < 1
        'has_mst':     conf.fitsout.mst      = (nword EQ 1) ? 0b : long(val) < 1
        'has_knownd':  conf.fitsout.knownd   = (nword EQ 1) ? 0b : long(val) < 1
        'has_hrds':    conf.fitsout.hrds     = (nword EQ 1) ? 0b : long(val) < 1
        
        ;; HISA Parameters
        'ta_cont':    conf.ta_cont          = (nword EQ 1) ? 0.d : double(val)
        'ta_hi':      conf.ta_hi            = (nword EQ 1) ? 0.d : double(val)
        ELSE:BEGIN        
           message,'Unknown keyword >'+key+'< on line '+$
                   strtrim(lnum,2)+' in conffile '+conffile,/cont
           RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
        END
     ENDCASE
  ENDWHILE
  
  RETURN,conf
END
