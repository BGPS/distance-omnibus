;+
; NAME:
;       OMNI_READ_CONFFILE_READLINE
;
; PURPOSE:
;       Subroutine of OMNI_READ_CONFFILE() to read a line from the
;       configuration file for parsing by one of the CTYPE-specific
;       subroutines.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       key = OMNI_READ_CONFFILE_READLINE( lun, conffile, lnum, val, 
;                                          nword, skip)
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
;       KEY -- Configuration file keyword for this line.
;
; OPTIONAL OUTPUTS:
;       VAL      -- Returned value associated with output KEY
;       NWORD    -- Number of 'words' in the line, used to determine
;                   if a VAL is present in the config file.
;       SKIP     -- Boolean, was this line blank and skipped?
;
; MODIFICATION HISTORY:
;
;       Created:  02/19/13, TPEB -- Initial version.
;       Modified: 02/21/13, TPEB -- Split out parsing of individual
;                                   config types into subfunctions.
;       Modified: 02/27/13, TPEB -- Moved key/val extraction to new
;                                   subroutine, added GALACTIC type
;                                   parsing.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;
;-

;;=========================================================================
;; Function for reading in a line of a config file, and returning the
;;   key and val -- placed as a subroutine because it's in EACH
;;   of the TYPE subroutines below.
FUNCTION OMNI_READ_CONFFILE_READLINE, lun, conffile, lnum, val, nword, skip
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  skip=0b
  line = ''
  lnum++
  readf,lun,line                ; read line
  line = strtrim(line,2)        ; clear whitespaces
  commpos = strpos(line,'#')    ; strip trailing comment
  IF commpos NE -1 THEN line = strmid(line,0,commpos)
  
  ;; Skip blank or comment lines
  IF line EQ '' || strmid(line,0,1) EQ '#' THEN BEGIN
     skip=1b
     RETURN,''
  ENDIF
  
  ;; Parse keyword from data
  words = strsplit(line,'=',/extract)
  nword = n_elements(words)
  CASE nword OF
     1: BEGIN
        key = strlowcase(strtrim(words[0],2))
        val = 1
     END 
     2: BEGIN
        key = strlowcase(strtrim(words[0],2))
        val = strtrim(words[1],2)
     END
     ELSE: BEGIN
        message,'Line '+strtrim(lnum,2)+' in conffile '+$
                conffile+' contains more than 1 = sign',/cont
        RETURN,{survey:'ERROR:',cat:'',error:'More than 1 equal sign'}
     END
  ENDCASE
  
  RETURN,key
END
