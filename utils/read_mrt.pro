;+
; NAME:
;       READ_MRT
;
; PURPOSE:
;       Wrapper for IDLASTRO routine READ_FMR, which reads in
;       ApJ MRT's.  This routine returns data as an array of
;       structures, rather than the mess that READ_FMR returns.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       str = READ_MRT(filename [,COUNT=count])
;
; INPUTS:
;       FILENAME -- Name of ApJ MRT to read in.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       STR -- Output array of structures, where each array element is
;              a structure containing information from one line of the
;              MRT.
;
; OPTIONAL OUTPUTS:
;       COUNT -- Number of elements in STR
;
; MODIFICATION HISTORY:
;
;       Created:  08/22/12, TPEB -- Initial Version.
;       Modified: 01/23/13, TPEB -- Added COUNT optional output.
;       Modified: 02/25/13. TPEB -- Updated documentation, and added
;                                   COMPILE_OPT
;
;-

FUNCTION READ_MRT, filename, COUNT=count
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  count = 0
  IF n_elements(filename) EQ 0 THEN BEGIN
     message,'Missing: required input filename.',/cont
     RETURN,0
  ENDIF
  IF ~FILE_TEST(filename) THEN BEGIN
     message,'File '+filename+' does not exist.',/cont
     RETURN,0
  ENDIF
  
  ;; Read in the table using IDLASTRO routine
  data = read_fmr(filename)
  
  ;; Create the output structure with structure tags
  ncp = n_elements(data.name)
  q = '"'
  
  type = size(data.data.(0)[0],/TNAME)
  IF strcmp(type,'INT',3) THEN type = 'LONG'
  command = 'str = create_struct(IDL_VALIDNAME(data.name[0],/CONVERT_ALL),'+$
            type+'('+q+q+'))'
  err = Execute(command)
  
  FOR i=1L, ncp-1 DO BEGIN
     type = size(data.data.(i)[0],/TNAME)
     IF strcmp(type,'INT',3) THEN type = 'LONG'
     command = 'str = create_struct(str,IDL_VALIDNAME(data.name[i],'+$
               '/CONVERT_ALL),'+type+'('+q+q+'))'
     err = Execute(command)
  ENDFOR
  
  str = replicate(str,n_elements(data.data.(0)))
  
  ;; Now, populate the structures
  FOR i=0L, ncp-1 DO str.(i) = data.data.(i)
  
  count = n_elements(str)
  RETURN,str
END
