;+
; NAME:
;       OMNI_READ_CONFFILE
;
; PURPOSE:
;       Reads a distance-omnibus configuration file and returns a
;       structure containing the fields in the file.
;
; CATEGORY:
;       distance-omnibus Configuration File Utility
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE( conffile )
;
; INPUTS:
;       CONFFILE -- Path of the configuration file to be read in.
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
;       Modified: 02/26/13, TPEB -- Added conf.pixscale and
;                                   conf.ppbeam for SURVEY.  Added
;                                   LOCAL type parsing.
;       Modified: 02/27/13, TPEB -- Moved key/val extraction to new
;                                   subroutine, added GALACTIC type
;                                   parsing.
;       Modified: 02/28/13, TPEB -- Added DPDF type parsing.
;       Modified: 03/01/13, TPEB -- Placed which/when DPDF run
;                                   information into substructure for
;                                   DPDF type config.
;       Modified: 03/04/13, TPEB -- Added FWHM keyword to SURVEY type
;                                   config file.
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
;       Modified: 04/03/13, TPEB -- Added mass-function parameters in
;                                   MASS type configuration file.
;       Modified: 04/17/13, TPEB -- For MASS type, changed temperature
;                                   distribution to lognormal from
;                                   Gaussian.
;       Modified: 05/03/13, TPEB -- Added mmax to MASS type.
;       Modified: 05/07/13, TPEB -- Added dmfbinw to MASS type.
;       Modified: 06/05/13, TPEB -- Added noise/hasnoise to SURVEY
;                                   type.
;       Modified: 08/12/13, TPEB -- Added ANCILLARY type configuration
;                                   file.
;       Modified: 08/21/13, TPEB -- Updating MASS type for use with
;                                   the GRSMATCH kinematic distance
;                                   likelihood DPDF.
;       Modified: 08/22/13, TPEB -- Added more fields to ANCILLARY.
;       Modified: 08/26/13, TPEB -- Added label map flag to
;                                   SURVEY. Also, force check of nword
;                                   EQ 1.
;       Modified: 09/03/13, TPEB -- Updating DPDF type for use with
;                                   the GRSMATCH kinematic distance
;                                   likelihood DPDF.
;       Modified: 09/18/13, TPEB -- Added KNOWND prior DPDF.
;       Modified: 10/23/13, TPEB -- Added hasvelocity and mirmult and
;                                   usegrs fields to SURVEY.
;       Modified: 11/01/13, TPEB -- Added MAKEEMAFPS to LOCAL, along
;                                   with locations of the MW 8-micron
;                                   model for EMAF.
;       Modified: 11/13/13, TPEB -- Added HRDS prior DPDF.
;       Modified: 11/18/13, TPEB -- Added GRS 13CO extraction values
;                                   to ANCILLARY.
;       Modified: 12/07/13, TPEB -- Added BESSEL and PPV fields to
;                                   ANCILLARY.
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;       Modified: 12/11/13, TPEB -- Added STEP_WIDTH to GALACITC for
;                                   roll-off width of the ERF() used
;                                   for step-function DPDFs.
;       Modified: 12/13/13, TPEB -- Added SAVE_ALL to DPDF for
;                                   specifying to save ALL objects to
;                                   the FITS bintable.
;       Modified: 02/06/14, TPEB -- Split out auxillary routines into
;                                   separate files, leaving only the
;                                   main routine in this file.
;       Modified: 03/11/14, TPEB -- Remove MASS type for release on
;                                   GITHUB.
;
;-

FUNCTION OMNI_READ_CONFFILE, conffile
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE

  ;; Parse input
  IF ~n_elements(conffile) THEN BEGIN
     message,'Error: No configuration file specified',/cont
     RETURN,{error:'No configuration file specified'}
  ENDIF
  ;; Check for existance of conffile
  IF ~FILE_TEST(conffile) THEN BEGIN
     message,'Error: Configuration file '+conffile+' not found',/cont
     RETURN,{error:'Configuration file not found'}
  ENDIF
  
  ;; Open the configuration file & read until keyword CTYPE
  openr,lun,conffile,/GET_LUN
  line = ''
  lnum = 0
  
  ;; Loop
  WHILE ~EOF(lun) DO BEGIN
     lnum++
     readf,lun,line             ; read line
     line = strtrim(line,2)     ; clear whitespaces
     commpos = strpos(line,'#') ; strip trailing comment
     IF commpos NE -1 THEN line = strmid(line,0,commpos)
     ;; Skip blank or comment lines
     IF line EQ '' || strmid(line,0,1) EQ '#' THEN CONTINUE
     
     ;; Parse keyword from data
     words = strsplit(line,'=',/extract)
     key = strtrim(words[0],2)
     IF key EQ 'ctype' THEN BEGIN
        ctype = strlowcase(strtrim(words[1],2))
        BREAK
     ENDIF
  ENDWHILE
  
  IF n_elements(ctype) EQ 0 THEN BEGIN
     message,'Error: No CTYPE keyword in '+conffile,/cont
     conf = {error:'No ctype keyword'}
  ENDIF ELSE $
     
     CASE ctype OF
     ;;=============================================
     ;; Parse SURVEY-INFO file into structure, and error-check
     'survey-info':BEGIN
        
        ;; Send to subroutine for actual parsing
        conf = omni_read_conffile_survey(lun,conffile,lnum)
        
        ;; Check for ERROR element having nonzero length
        IF strlen(conf.error) THEN BEGIN
           message,'Error: '+conf.error,/cont
           BREAK
        ENDIF
        
        ;; Check that the keyword 'survey' is properly set, else return error
        IF conf.survey EQ '' || strtrim(conf.survey,2) EQ '1' THEN BEGIN
           message,'Error: SURVEY keyword not set in '+conffile,/cont
           conf = {error:'Survey keyword not set in survey_info type file'}
           BREAK
        ENDIF
        
        ;; Check that the keyword 'cat' is properly set, else return error
        IF conf.cat EQ '' || strtrim(conf.survey,2) EQ '1' THEN BEGIN
           message,'Error: CAT keyword not set in '+conffile,/cont
           conf = {error:'Cat keyword not set in survey_info type file'}
           BREAK
        ENDIF
        
        ;; Check that the keyword 'cat' points to a file, else return error
        IF ~FILE_TEST(conf.cat,/read) THEN BEGIN
           message,'Error: CAT keyword in '+conffile+' is not a file',/cont
           conf = {error:'Cat keyword not valid file'}
           BREAK
        ENDIF
        
     END                        ; End of survey-info CASE statement
     
     
     ;;=============================================
     ;; Parse GALACTIC-PARAMS file and create structure
     'galactic-params':BEGIN
        
        ;; Send to subroutine for actual parsing
        conf = omni_read_conffile_galactic(lun,conffile,lnum)
        
        ;; Check that the keyword 'r0' is properly set, else return error
        IF conf.r0 EQ '' || strtrim(conf.r0,2) EQ '1' THEN BEGIN
           message,'Error: R0 keyword not set in '+conffile,/cont
           conf = {error:'R0 keyword not set in galactic_params type file'}
           BREAK
        ENDIF
        
        ;; Check that the keyword 'curve' is properly set, else return error
        IF conf.curve EQ '' || strtrim(conf.curve,2) EQ '1' THEN BEGIN
           message,'Error: CURVE keyword not set in '+conffile,/cont
           conf = {error:'CURVE keyword not set in galactic_params type file'}
           BREAK 
        ENDIF
        
     END                        ; End of galactic-params CASE statement
     
     
     ;;=============================================
     ;; Parse DPDF-PARAMS file and create structure
     'dpdf-params':BEGIN
        
        ;; Send to subroutine for actual parsing
        conf = omni_read_conffile_dpdf(lun,conffile,lnum)
        
        ;; Check that the keyword 'nbins' is properly set, else return error
        IF conf.nbins EQ '' || strtrim(conf.nbins,2) EQ '1' THEN BEGIN
           message,'Error: NBINS keyword not set in '+conffile,/cont
           conf = {error:'NBINS keyword not set in dpdf_params type file'}
           BREAK 
        ENDIF
        
     END                        ; End of dpdf-params CASE statement
     
     
     ;;=============================================
     ;; Parse LOCAL-LAYOUT file and create structure
     'local-layout':BEGIN
        
        ;; Send to subroutine for actual parsing
        conf = omni_read_conffile_local(lun,conffile,lnum)
        
        ;; Check that the keyword 'postage' is properly set, else return error
        IF conf.postage EQ '' || strtrim(conf.postage,2) EQ '1' THEN BEGIN
           message,'Error: POSTAGE keyword not set in '+conffile,/cont
           conf = {error:'Postage keyword not set in local_layout type file'}
           BREAK
        ENDIF
        
     END                        ; End of local-layout CASE statement
     
     
     ;;=============================================
     ;; Parse ANCILLARY file and create structure
     'ancillary':BEGIN
        
        ;; Send to subroutine for actual parsing
        conf = omni_read_conffile_ancillary(lun,conffile,lnum)
        
        ;; Check that the keyword 'postage' is properly set, else return error
        IF conf.grs_rad EQ 0. || strtrim(conf.grs_rad,2) EQ '1' THEN BEGIN
           message,'Error: GRS_RAD keyword not set in '+conffile,/cont
           conf = {error:'GRS_RAD keyword not set in ancillary type file'}
           BREAK
        ENDIF
        
     END                        ; End of ancillary CASE statement
     
     
     ;;=============================================
     ;; Else, return error
     ELSE: BEGIN
        message,'Error: Invalid ctype '+ctype+' in conffile '+conffile,/cont
        conf = {error:'Invalid ctype'}
        BREAK
     END
  ENDCASE
  
  ;; Close the file and free pointer
  close,lun
  free_lun,lun
  
  RETURN,conf
END
