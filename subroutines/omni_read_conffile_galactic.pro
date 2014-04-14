;+
; NAME:
;       OMNI_READ_CONFFILE_GALACTIC
;
; PURPOSE:
;       Parses the GALACTIC-type configuration file.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE_GALACTIC( lun, conffile, lnum )
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
;       Modified: 02/25/13, TPEB -- Adding CASE statements for the
;                                   velocity information
;       Modified: 02/27/13, TPEB -- Moved key/val extraction to new
;                                   subroutine, added GALACTIC type
;                                   parsing.
;       Modified: 12/11/13, TPEB -- Added STEP_WIDTH to GALACITC for
;                                   roll-off width of the ERF() used
;                                   for step-function DPDFs.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;
;-

;;=========================================================================
;; Function for the parsing of galactic-params configuration files.
FUNCTION OMNI_READ_CONFFILE_GALACTIC, lun, conffile, lnum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Create the configuration structure for TYPE galactic
  conf = {r0:0.d,$
          v0:0.d,$
          vs:0.d,$
          errvlsr:0.d,$
          z0:0.d,$
          bigu:0.d,$
          bigv:0.d,$
          bigw:0.d,$
          curve:'',$
          td:0.d,$
          gas2dust:0.d,$
          step_width:0.d,$
          error:''}
  
  ;; Continue reading the configuration file
  WHILE ~EOF(lun) DO BEGIN
     
     key = OMNI_READ_CONFFILE_READLINE(lun, conffile, lnum, val, nword, skip)
     IF skip THEN CONTINUE
     
     CASE key OF
        'r0':         conf.r0         = (nword EQ 1) ? 0.d : double(val)
        'v0':         conf.v0         = (nword EQ 1) ? 0.d : double(val)
        'vs':         conf.vs         = (nword EQ 1) ? 0.d : double(val)
        'errvlsr':    conf.errvlsr    = (nword EQ 1) ? 0.d : double(val)
        'z0':         conf.z0         = (nword EQ 1) ? 0.d : double(val)
        'bigu':       conf.bigu       = (nword EQ 1) ? 0.d : double(val)
        'bigv':       conf.bigv       = (nword EQ 1) ? 0.d : double(val)
        'bigw':       conf.bigw       = (nword EQ 1) ? 0.d : double(val)
        'curve':      conf.curve      = (nword EQ 1) ? ''  : strupcase(val)
        'td':         conf.td         = (nword EQ 1) ? 0.d : double(val)
        'gas2dust':   conf.gas2dust   = (nword EQ 1) ? 0.d : double(val)
        'step_width': conf.step_width = (nword EQ 1) ? 0.d : double(val)
        ELSE:BEGIN        
           message,'Unknown keyword >'+key+'< on line '+$
                   strtrim(lnum,2)+' in conffile '+conffile,/cont
           RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
        END
     ENDCASE
  ENDWHILE
  
  RETURN,conf
END
