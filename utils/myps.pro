;+
; NAME:
;       MYPS
;
; PURPOSE:
;       Lazy shortcut for all the junk needed to open and close .eps
;       files, plus changing the fonts and plot regimes.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       MYPS [,filename][,/DONE][,/MP]
;
; INPUTS:
;       FILENAME -- Name of the PostScript file to be created.
;
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       DONE -- Close the PostScript file and return to X
;       MP   -- In conjunction with /DONE, cleans up multiplot by
;               resetting to default parameters.
;
; OUTPUTS:
;       NONE (Opens / closes a PostScript file)
;
; OPTIONAL OUTPUTS:
;       NONE  (Passes on anything else in the _EXTRA command to mydevice)
;
; MODIFICATION HISTORY:
;
;       Created:  11/08/11, TPEB -- Finally got sick of typing
;                                   set_plot,'ps'.
;       Modified: 01/31/12, TPEB -- Added ![xy].THICK settings to make
;                                   postscript plots have darker
;                                   borders automatically.
;       Modified: 06/21/12. TPEB -- Added PSYM=8 == filled circle as
;                                   part of plot initialization, and
;                                   /MP keyword to clean up multiplot
;                                   automatically.
;       Modified: 12/11/12, TPEB -- Cleaned up ordering of /DONE
;                                   section, and made ON_ERROR,2.
;       Modified: 05/06/13, TPEB -- Added COMPILE_OPT statement.
;       Modified: 07/23/13, TPEB -- Set !p.thick = 3 because
;                                   I'm sick of always having
;                                   to specify it in plot commands!
;
;-

PRO MYPS, filename, DONE=done, MP=mp, _EXTRA=extra
  
  COMPILE_OPT IDL2
  
  ON_ERROR,2
  
  IF KEYWORD_SET( done ) THEN BEGIN
     IF KEYWORD_SET( mp ) THEN BEGIN
        multiplot,/reset
        multiplot,/default
        multiplot,/reset
     ENDIF
     device,/close_file
     !p.font = -1
     set_plot,'x'
     !x.thick = 0
     !y.thick = 0
     !p.thick = 0
  ENDIF ELSE BEGIN
     set_plot,'ps'
     !p.font = 0
     mydevice,filename,_EXTRA=extra
     !x.thick = 3
     !y.thick = 3
     !p.thick = 3
  ENDELSE
END
