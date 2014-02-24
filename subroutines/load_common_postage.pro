PRO LOAD_COMMON_POSTAGE
  
  COMMON POSTAGE_BLOCK, common_img, common_hdr
  
  UBC_DIR = './local/ubc_glimpse_proc/'
  
  s = read_bgps_csv()
  
  fn = FILE_SEARCH(UBC_DIR+'bgps????.fits',count=nubc)
  print,nubc
  
  ptr = strpos(fn[0],'.fits')
  scnum = strmid(fn,ptr-4,4)
  
  ;; Load Test images
  bgps = readfits(UBC_DIR+'bgps'+scnum[0]+'.fits',bgpshdr, $
                  /silent) * 1.5d 
  Imir = readfits(UBC_DIR+'Imir'+scnum[0]+'.fits',imirhdr, /silent)
  irac = readfits(UBC_DIR+'smooth33arc'+scnum[0]+'.fits',irachdr, /silent)
  labl = readfits(UBC_DIR+'label'+scnum[0]+'.fits',labhdr, /silent)
  
  common_hdr = replicate(create_struct('bgps',strarr(150),$
                                       'imir',strarr(150),$
                                       'irac',strarr(150),$
                                       'labl',strarr(150)),nubc)
  
  bsz = size(bgps,/DIM)
  
  common_img = replicate(create_struct('cnum',0L,$
               'bgps',dblarr((size(bgps,/DIM))[0],(size(bgps,/DIM))[1]),$
               'imir',fltarr((size(Imir,/DIM))[0],(size(Imir,/DIM))[1]),$
               'irac',fltarr((size(irac,/DIM))[0],(size(irac,/DIM))[1]),$
               'labl',bytarr((size(labl,/DIM))[0],(size(labl,/DIM))[1])),$
                         nubc)
  
  FOR i=0L,nubc-1 DO BEGIN
     IF (i+1) MOD 200 EQ 0 THEN $
        message,'Compiling UBC POSTAGE for #'+string(i+1,format="(I4)")+$
                ' of '+string(nubc,format="(I4)"),/inf
     
     tsz = size(readfits(UBC_DIR+'bgps'+scnum[i]+'.fits',/silent),/DIM)
     
     
     IF tsz[0] EQ bsz[0] && tsz[1] EQ bsz[1] THEN BEGIN
        
        common_img[i].cnum = long(scnum[i])
        
        common_img[i].bgps = $
           readfits(UBC_DIR+'bgps'+scnum[i]+'.fits',bgpshdr, /silent) * 1.5d 
        common_img[i].Imir = $
           readfits(UBC_DIR+'Imir'+scnum[i]+'.fits',imirhdr, /silent)
        common_img[i].irac = $
           readfits(UBC_DIR+'smooth33arc'+scnum[i]+'.fits',irachdr, /silent)
        common_img[i].labl = $
           readfits(UBC_DIR+'label'+scnum[i]+'.fits',labhdr, /silent)
        
        ;; print,n_elements(bgpshdr),n_elements(imirhdr),n_elements(irachdr),$
        ;;       n_elements(labhdr)
        common_hdr[i].bgps[0:n_elements(bgpshdr)-1] = bgpshdr
        common_hdr[i].imir[0:n_elements(imirhdr)-1] = imirhdr
        common_hdr[i].irac[0:n_elements(irachdr)-1] = irachdr
        common_hdr[i].labl[0:n_elements(labhdr) -1] = labhdr
        
     ENDIF ELSE message,'Skipped BGPS #'+scnum[i]+' size mismatch.',/inf
  ENDFOR
END
