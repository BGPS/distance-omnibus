Function Rascii, filnam, buffer = buf, double = doub, $
    npoints = ncr, header = head, show = sho, skip = skip, error_status = err

;+
; NAME:
;	RASCII
; VERSION:
;	3.0
; PURPOSE:
;	Reads data from an ASCII file into an array.  It is assumed that the 
;	file contains columns of numbers, with the same number of entries in
;	each row.  The numbers may be separated by commas, spaces and/or tabs.
;	The file may contain a header.  The first line in which the first
;	non-blank character is one of ".+-0123456789" will be considered the
;	beginning of the data.  Text lines imbedded in the data are skipped.
; CATEGORY:
;	Input/Output.
; CALLING SEQUENCE:
;	Result = RASCII( FILNAM [, optional keywords])
; INPUTS:
;    FILNAM
;	Char. value, the name of the data file.  Default extension is '.DAT'.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    BUFFER
;	Initial number of data rows.  Default is 256.  In any case the result 
;	Is trimmed to the actual number.
;    /DOUBLE
;	If set, the data is input as DOUBLE.  Default is FLOAT.
;    /SHOW
;	If set, the header (if one exists) will be printed to the screen.
;    NPOINTS
;	Optional output, see below.
;    HEADER
;	Optional output, see below.
;    SKIP
;	Number of lines to skip at the beginning of the file.  This keyword can 
;	be used if the header of the file contains lines beginning with 
;	".+-0123456789" which would otherwise be read as data.  Default is 0.
; OUTPUTS:
;	Returns the data in a (NC,NR) floating (or double precision if DOUBLE 
;	is set) array, where NC, NR, are the numbers of columns and rows,
;	respectively.  In case of error returns 0.
; OPTIONAL OUTPUT PARAMETERS:
;    NPOINTS
;	The name of a 2-dim vector to receive the values of NC, NR (see above).
;	Doesn't need to be defined prior to the call.  In case of an error 
;	returns [0,0].
;    HEADER
;	The name of a character array to receive the header lines.  Doesn't 
;	need to be defined prior to the call.  In case of an error, or if no
;	header exists, returns a zero length string.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses DEFAULT, STREQ and STRPARSE from MIDL.
; MODIFICATION HISTORY:
;	Created 25-JAN-1992 by Mati Meron.
;	Modified 25-MAY-1994 by Mati Meron.  Added buffering and DOUBLE option.
;	Modified 14-FEB-1995 by Mark Rivers.  Added keyword SKIP.
;	Modified 24-JUL-1995 by Mati Meron.  Removed a UNIX/VMS conflict in the 
;	file OPENR statement.
;	Modified 10-JUL-1997 by Mati Meron.  Name changed from READ_ASCII to
;	RASCII to avoid conflict with the READ_ASCII routine in IDL ver. 5.
;	Modified 12-SEP-1997 by Mati Meron.  Added keyword ERROR_STATUS, 
;	following a modification by Roger Dejus, XFD/APS.
;-

    err = 0l
    ncr = [0l,0l]
    bufsiz = (round(Default(buf,256)/256.) > 1l)*256l
    nrtem = bufsiz
    dtyp = 4 + keyword_set(doub)
    on_ioerror, file_no_good
    if Streq(!version.os,'vms',3) then begin
	openr, datun, filnam, default = '.dat', /get_lun
    endif else openr, datun, filnam, /get_lun

    line = ''
    head = ''
    nc = 0l
    on_ioerror, data_no_good
    if n_elements(skip) ne 0 then for i = 0l, skip-1 do $
	readf, datun, line, prompt = ''
    while nc eq 0 and not eof(datun) do begin
	finf = fstat(datun)
	readf, datun, line, prompt = ''
	if line ne '' then begin
	    bline = byte(strtrim(line,1))
	    if Strparse_mm(' .+-0123456789 ',string(bline(0))) eq 0 then begin
		head = [temporary(head),line]
		if keyword_set(sho) then print, line
	    endif else nc = 1l + Strparse_mm(line, '	, ')
	endif
    endwhile
    head = transpose(head(n_elements(head) gt 1:*))

    if nc gt 0 then begin
	point_lun, datun, finf.cur_ptr
	datline = make_array(nc, type = dtyp)
	data = make_array(nc,bufsiz, type = dtyp)
	on_ioerror, next
	nr = 0l
	next:
	while not eof(datun) do begin
	    readf, datun, datline, prompt = ''
	    data(*,nr) = datline
	    nr = nr + 1l
	    if nr eq nrtem then begin
		data = [[data],[make_array(nc,bufsiz, type = dtyp)]]
		nrtem = nrtem + bufsiz
	    endif
	endwhile
	data = data(*,0:nr-1)
	ncr = [nc,nr]
    endif else data = 0

    free_lun, datun
    return, data

    data_no_good:
    free_lun, datun
    file_no_good:
    print, !err_string
    err = !error
    return, 0

end
