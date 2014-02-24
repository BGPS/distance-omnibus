;+
; NAME:
;       SGPS_HACK_CONTINUUM_MAPS
;
; PURPOSE:
;       The SGPS group has not released (and aparently will not ever
;       release) T_A calibrated 21-cm continuum maps for their portion
;       of the Galactic plane.  Hence, this routine is designed to
;       hack the HI line data cubes and create a pseudo-continuum map
;       from the (line-emission free) region of large negative velocity.
;
; CATEGORY:
;       SGPS Utility
;
; CALLING SEQUENCE:
;       SGPS_HACK_CONTINUUM_MAPS [,sgps_list]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       SGPS_LIST -- Filename of list of SGPS HI data cubes
;                    [Default: ./local/sgpa_list.txt]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       Routine creates two-dimensional FITS files in the same
;       directory as the HI cubes, with filename prepended with
;       "cont_".
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  05/01/11, TPEB -- Initial version.
;       Modified: 06/15/11, TPEB -- Changed DELVARX to UNDEFINE.
;       Modified: 02/27/13, TPEB -- Converted routine to PRO, updated
;                                   documentation, and other changes
;                                   to conform to new, generalized
;                                   distance-omnibus code base.
;
;-

PRO SGPS_HACK_CONTINUUM_MAPS, sgps_list
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  ;; Check SGPS_LIST
  IF n_elements(sgps_list) EQ 0 THEN BEGIN
     local = omni_read_conffile('./conffiles/local_layout.conf')
     sgps_list = local.sgps
  ENDIF
  
  ;; Read in list
  readcol,sgps_list,fn,format='a',comment='#',/silent
  
  path = strmid(fn[0],0,strpos(fn[0],'/',/reverse_search)+1)
  fn1 = strmid(fn,strpos(fn[0],'/',/reverse_search)+1)
  
  ind = where( strmid(fn1,0,1) EQ 'g', n_hi)
  IF n_hi NE 0 THEN BEGIN
     hi_cubes = fn[ind]
     fn1      = fn1[ind]
  ENDIF ELSE message,'RUM!'
  
  ;; Why not?  Print!
  print,hi_cubes,n_hi
  
  FOR i=0L, n_hi-1 DO BEGIN
     
     UNDEFINE,cube
     cube = readfits(hi_cubes[i], hdr)
     rdhd, hdr, s = h
     
     extast,hdr,hiastr
     hisize = (size(cube,/DIM))[0:1]
     
     print,minmax(h.v)
     
     ;; Clear header for making continuum files
     junk = hdr
     sxdelpar, junk, 'NAXIS3'
     sxdelpar, junk, 'NAXIS4'
     sxaddpar, junk, 'NAXIS', 2
     
     ;; Use HEXTRACT to extract the subsection header, and update hdr WCS
     hextract,cube[*,*,0],junk,ps,cont_hdr,0,hisize[0]-1,0,hisize[1]-1
     
     ;; Check for which side of the Galaxy we're on, set velocity accordingly
     usev = min(h.v) + 50.
     
     z_ind = WHERE(h.v LE usev, nzind)
     print,'NZIND: ',nzind
     
     ;; Take it as the median of this range
     contmap = median( cube[*,*,z_ind], DIM=3)
     
     ;; Write the file
     mwrfits,contmap,path+'cont_'+fn1[i],cont_hdr,/create
     
  ENDFOR
  
END
