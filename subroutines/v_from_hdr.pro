;+
; NAME:
;       V_FROM_HDR
;
; PURPOSE:
;       Extract the velocity scale from a 1-D FITS spectrum or other
;       FITS file whose first dimension is velocity.  Originally
;       designed to operate on densegas spectra FITS files compiled by
;       Adam Ginsburg, but expanded to be more general.
;
; CATEGORY:
;       distance-omnibus utility
;
; CALLING SEQUENCE:
;       v_arr = v_from_hdr(hdr)
;
; INPUTS: 
;       HDR  -- Header from the FITS file, read in by readfits or
;               headfits.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       V_ARR -- The velocity array corresponding to the spectral
;                emission spectrum contained as data in the FITS file.
;
; OPTIONAL OUTPUTS:
;       DV    -- The channel spacing in the velocity array.
;
; MODIFICATION HISTORY:
;
;       Created:  10/17/10, TPEB -- Initial Version
;       Modified: 07/08/11, TPEB -- Name change, plus adding pathway
;                                   for extracting velocity from the
;                                   frequency scale of FITS files
;                                   output by CLASS.
;       Modified: 08/30/11, TPEB -- Added (commented-out) debugging
;                                   code, plus fixed typo in
;                                   definition of c -- was off by 2
;                                   m/s.
;       Modified: 11/08/11, TPEB -- Added DV optional output to return
;                                   the channel width in the spectrum.
;-

FUNCTION V_FROM_HDR, hdr, DV=dv
  
  ;; Extract universal header values
  nvals = sxpar(hdr,'NAXIS1')
  crval = sxpar(hdr,'CRVAL1')
  crpix = sxpar(hdr,'CRPIX1')
  ;; Check for whether using CD1_1 or CDELT1, then make variable cdelt
  cd11  = sxpar(hdr,'CD1_1', count=ncd)
  cdel  = sxpar(hdr,'CDELT1', count=ndel)
  cdelt = float(ncd * cd11) + float(ndel * cdel)

  ;; Debugging!
  ;; IF crval NE 0. THEN message,$
  ;;    string(sxpar(hdr,'LINE'),sxpar(hdr,'OBJECT'),$
  ;;           format="('Fixed ',A0,' spectrum for object ',A0)"),/inf
  
  ;; Next, check whether this file comes from CLASS or not.
  IF strcmp(strtrim(sxpar(hdr, 'ORIGIN'),2),'CLASS',5) OR $
     strcmp(strtrim(sxpar(hdr, 'CTYPE1'),2),'FREQ',4) THEN BEGIN 

     ;; CLASS files are in frequency
     ;; v = (nu0 - nu)/nu0 * c
     ;; freqs are nu - n0, so need additional '-'.
     nu0   = sxpar(hdr,'RESTFREQ')
     vels = -((findgen(nvals)-(crpix-1))*cdelt + crval)*2.99792458d5 / nu0
     
  ENDIF ELSE BEGIN
     
     vels = (findgen(nvals)-(crpix-1))*cdelt + crval
     
     ;; Check for 'M/S' unit
     unit = sxpar(hdr,'CUNIT1')
     IF strcmp(strtrim(unit,2),'M/S',3,/FOLD_CASE) THEN vels /= 1.d3
     ;; Else, assume velocity in KM/S
     
  ENDELSE
  dv = (vels[1:*] - vels[0:*])[0]
  
  RETURN, vels
END
