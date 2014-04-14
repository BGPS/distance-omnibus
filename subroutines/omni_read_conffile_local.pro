;+
; NAME:
;       OMNI_READ_CONFFILE_LOCAL
;
; PURPOSE:
;       Parses the LOCAL-type configuration file.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE_LOCAL( lun, conffile, lnum )
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
;       Modified: 02/26/13, TPEB -- Added conf.pixscale and
;                                   conf.ppbeam for SURVEY.  Added
;                                   LOCAL type parsing.
;       Modified: 02/27/13, TPEB -- Moved key/val extraction to new
;                                   subroutine, added GALACTIC type
;                                   parsing.
;       Modified: 11/01/13, TPEB -- Added MAKEEMAFPS to LOCAL, along
;                                   with locations of the MW 8-micron
;                                   model for EMAF.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;
;-

;;=========================================================================
;; Function for the parsing of local-layout configuration files.
FUNCTION OMNI_READ_CONFFILE_LOCAL, lun, conffile, lnum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Create the configuration structure for TYPE local
  conf = {hasglimpse:0b,$
          hasgrs:0b,$
          hasvgps:0b,$
          hascgps:0b,$
          hassgps:0b,$
          glimpse:'',$
          grs:'',$
          vgps:'',$
          cgps:'',$
          sgps:'',$
          postage:'',$
          pssize:0.,$
          emafpost:'',$
          gpssize:0.,$
          mwffore:'',$
          mwinten:'',$
          makeemafps:0b,$
          error:''}
  
  ;; Continue reading the configuration file
  WHILE ~EOF(lun) DO BEGIN
     
     key = OMNI_READ_CONFFILE_READLINE(lun, conffile, lnum, val, nword, skip)
     IF skip THEN CONTINUE
     
     CASE key OF
        'postage':    conf.postage    = val
        'pssize':     conf.pssize     = (nword EQ 1) ? 0. : float(val)
        'emafpost':   conf.emafpost   = val
        'gpssize':    conf.gpssize    = (nword EQ 1) ? 0. : float(val)
        'mwffore':    conf.mwffore    = val
        'mwinten':    conf.mwinten    = val
        'makeemafps': conf.makeemafps = (nword EQ 1) ? 0b : long(val) < 1
        'glimpse': BEGIN
           IF nword NE 2 THEN BREAK
           IF ~FILE_TEST(val,/read) THEN BREAK
           conf.hasglimpse = 1b
           conf.glimpse = val
        END
        'grs': BEGIN
           IF nword NE 2 THEN BREAK
           IF ~FILE_TEST(val,/read) THEN BREAK
           conf.hasgrs = 1b
           conf.grs = val
        END
        'vgps': BEGIN
           IF nword NE 2 THEN BREAK
           IF ~FILE_TEST(val,/read) THEN BREAK
           conf.hasvgps = 1b
           conf.vgps = val
        END
        'cgps': BEGIN
           IF nword NE 2 THEN BREAK
           IF ~FILE_TEST(val,/read) THEN BREAK
           conf.hascgps = 1b
           conf.cgps = val
        END
        'sgps': BEGIN
           IF nword NE 2 THEN BREAK
           IF ~FILE_TEST(val,/read) THEN BREAK
           conf.hassgps = 1b
           conf.sgps = val
        END
        ELSE:BEGIN        
           message,'Unknown keyword >'+key+'< on line '+$
                   strtrim(lnum,2)+' in conffile '+conffile,/cont
           RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
        END
     ENDCASE
  ENDWHILE
  
  RETURN,conf
END
