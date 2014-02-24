;+
; NAME:
;       MEDIAN_IMCOMBINE
;
; PURPOSE:
;       Combine images using a median filter to reject outlier pixels.
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       outimg = MEDIAN_IMCOMBINE(img1,img2,img3,...)
;
; INPUTS:
;       IMG1 -- 1st image to be combined
;        .
;        .
;       IMG15 -- 15th image to be combined.  May use any number of
;                images from 1 to 15.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       OUTIMG -- Output, median-combined image
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  09/24/12, TPEB -- Initial version
;
;-

FUNCTION MEDIAN_IMCOMBINE, img1, img2, img3, img4, img5, img6, img7, img8, $
                           img9, img10, img11, img12, img13, img14, img15, $
                           NIMG=nimg, PLOT=plot
  
  plot = KEYWORD_SET(plot)
  ;; Figure out how many input images there are
  IF n_elements(nimg) EQ 0 THEN BEGIN
     nimg = 0
     FOR i=1,15 DO BEGIN
        command = 'nimg += KEYWORD_SET(img'+string(i,format="(I0)")+')'
        err = Execute(command)
     ENDFOR
  ENDIF
  
  IF nimg EQ 0 THEN BEGIN
     message,'Must supply at least one image!',/cont
     RETURN,0
  ENDIF
  
  ;; Next, check that the image sizes are all the same
  sz = size(img1,/DIM)
  FOR j=2,nimg DO BEGIN
     command = 'szi = size(img'+string(j,format="(I0)")+',/DIM)'
     err = Execute(command)
     IF ~ ARRAY_EQUAL(sz,szi) THEN BEGIN
        message,'All images must be the same size!',/cont
        RETURN,0
     ENDIF
  ENDFOR
  
  ;; Build 3D image array 
  img_3d = dblarr(sz[0],sz[1],nimg)
  
  ;; Place input images into the 3D array
  FOR k=0,nimg-1 DO BEGIN
     command = 'img_3d[*,*,k] = img'+string(k+1,format="(I0)")
     err = Execute(command)
  ENDFOR

  IF plot THEN BEGIN
     FOR ll=0,nimg-1 DO BEGIN
        cgloadct,3,/silent
        plotimage,img_3d[*,*,ll],range=set_plot_range(img_3d[*,*,ll]),$
                  /preserve,title = 'Slice #'+string(ll+1,format="(I0)")
        cgColorbar,range=set_plot_range(img_3d[*,*,ll]),$
                   title='Intensity [MJy/sr]'
        wait,0.5
     ENDFOR
  ENDIF
  
  IF nimg NE 1 THEN $
     outimg = median(img_3d, DIM=3, /EVEN) ELSE $
        outimg = img1
  
  IF plot THEN BEGIN
     cgloadct,3,/silent
     plotimage,outimg,range=set_plot_range(outimg),/preserve,$
               title = 'Output Image'
     cgColorbar,range=set_plot_range(outimg),title='Intensity [MJy/sr]'
     wait,0.5
  ENDIF
  
  maxval = max(outimg,indh)
  
  vals = dblarr(nimg+1)
  vals[0] = maxval
  for p=1,nimg do begin
     command = 'vals[p] = img'+string(p,format="(I0)")+'[indh]'
     err = execute(command)
  endfor
  ;; print,vals
  
  
  RETURN,outimg
END
