;+
; NAME:
;       OMNI_READ_CONFFILE_ANCILLARY
;
; PURPOSE:
;       Parses the ANCILLARY-type configuration file.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE_ANCILLARY( lun, conffile, lnum )
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
;       Modified: 08/12/13, TPEB -- Added ANCILLARY type configuration
;                                   file.
;       Modified: 08/22/13, TPEB -- Added more fields to ANCILLARY.
;       Modified: 11/18/13, TPEB -- Added GRS 13CO extraction values
;                                   to ANCILLARY.
;       Modified: 12/07/13, TPEB -- Added BESSEL and PPV fields to
;                                   ANCILLARY.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;
;-

;;=========================================================================
;; Function for the parsing of ancillary configuration files.
FUNCTION OMNI_READ_CONFFILE_ANCILLARY, lun, conffile, lnum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Create the configuration structure for TYPE galactic
  conf = {grs_rad:0.d,$
          grs_ta:0.d,$
          grs_ratio:0.d,$
          hi_rad:0.d,$
          hi_ta:0.d,$
          bessel:'',$
          ppv_dis:0.d,$
          ppv_dv:0.d,$
          error:''}
  
  ;; Continue reading the configuration file
  WHILE ~EOF(lun) DO BEGIN
     
     key = OMNI_READ_CONFFILE_READLINE(lun, conffile, lnum, val, nword, skip)
     IF skip THEN CONTINUE
     
     CASE key OF
        'grs_rad':   conf.grs_rad   = (nword EQ 1) ? 0.d : double(val)
        'grs_ta':    conf.grs_ta    = (nword EQ 1) ? 0.d : double(val)
        'grs_ratio': conf.grs_ratio = (nword EQ 1) ? 0.d : double(val)
        'hi_rad':    conf.hi_rad    = (nword EQ 1) ? 0.d : double(val)
        'hi_ta':     conf.hi_ta     = (nword EQ 1) ? 0.d : double(val)
        'bessel':    conf.bessel    = (nword EQ 1) ? ''  : val
        'ppv_dis':   conf.ppv_dis   = (nword EQ 1) ? 0.d : double(val)
        'ppv_dv':    conf.ppv_dv    = (nword EQ 1) ? 0.d : double(val)
        ELSE:BEGIN        
           message,'Unknown keyword >'+key+'< on line '+$
                   strtrim(lnum,2)+' in conffile '+conffile,/cont
           RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
        END
     ENDCASE
  ENDWHILE
  
  RETURN,conf
END
