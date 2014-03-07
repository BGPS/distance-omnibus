;+
; NAME:
;       OMNI_GLIMPSE_STARSUB
;
; PURPOSE:
;       Subtract stars from the IRAC Band 4 GLIMPSE images based on
;       stars found in the corresponding Band 1 image.  Allows for
;       clearer estimation of the diffuse 8-um Galactic emission.
;
;       WARNING: Running this procedure is extremely CPU-intensive,
;       possibly requiring days to complete.  See the NOTES section
;       below for a manual parallization scheme for a many-CPU machine
;       to speed processing.
;
;       NOTE: Once the star-subtracted GLIMPSE images have been
;       computed and stored, this routine need not be run ever again.
;
; CATEGORY:
;       distance-omnibus Local File Creation (EMAF-specific)
;
; CALLING SEQUENCE:
;       OMNI_GLIMPSE_STARSUB
;
; INPUTS:
;       NONE (all inputs hardwired or available in the configuration
;             file)
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE (star subtracted images written to disk)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; NOTES:
;       For a poor-man's parallelization, have added START &
;       REAR keywords to split processing of the 46 GLIMPSE tiles.  To
;       help evenly split compute time, listed below are the relative
;       percentages of stars in the GLIMPSE dataset. (With H_MIN = 10.)
;         #  GLON    NSTAR     %    % of 9
;         ---------------------------------
;         1  000:  2166518   8.0    71.8  -   1-1
;         2  003:  1643526   6.1    54.5  -   2-4
;         3  006:  1079814   4.0    35.8  
;         4  009:   849059   3.1    28.1  ^
;         5  012:   774383   2.9    25.7  -   5-8
;         6  015:   772110   2.8    25.6  
;         7  018:   772357   2.8    25.6
;         8  021:   762387   2.8    25.3  ^
;         9  024:   716840   2.6    23.8  -   9-13
;        10  027:   743235   2.7    24.6  
;        11  030:   620998   2.3    20.6  
;        12  033:   611332   2.3    20.3
;        13  036:   506003   1.9    16.8  ^
;        14  039:   442968   1.6    14.7  -   14-24
;        15  042:   424042   1.6    14.0
;        16  045:   390117   1.4    12.9  
;        17  048:   362669   1.3    12.0  
;        18  051:   320627   1.2    10.6
;        19  054:   302757   1.1    10.0
;        20  057:   267490   1.0     8.9
;        21  060:   239746   0.9     7.9
;        22  063:   205884   0.8     6.8
;        23  066:    59348   0.2     2.0
;        24  284:    26592   0.1     0.9  ^
;        25  294:    45899   0.2     1.5  -   25-33
;        26  297:   246679   0.9     8.2
;        27  300:   278706   1.0     9.2
;        28  303:   318361   1.2    10.5
;        29  306:   390146   1.4    12.9  
;        30  309:   415770   1.5    13.8  
;        31  312:   398688   1.5    13.2
;        32  315:   401427   1.5    13.3
;        33  318:   430395   1.6    14.3  ^
;        34  321:   465632   1.7    15.4  -   34-39
;        35  324:   484961   1.8    16.1
;        36  327:   500913   1.8    16.6  
;        37  330:   556374   2.0    18.4  
;        38  333:   556374   2.1    18.9
;        39  336:   659769   2.4    21.9  ^
;        40  339:   671631   2.5    22.3  -   40-44
;        41  342:   600311   2.2    19.9  
;        42  345:   588504   2.2    19.5  
;        43  348:   698643   2.6    23.1
;        44  351:   733773   2.7    24.3  ^
;        45  354:  1026747   3.8    34.0  -   45-46
;        46  357:  1620081   6.0    53.7  ^
;
; MODIFICATION HISTORY:
;
;       Created:  xx/xx/xx, Steve Mairs -- Initial verision, called
;                                          generalstarsub.pro in code
;                                          retrieved from
;                                          signals.siglab.ok.ubc.ca
;       Modified: 12/21/11, TPEB -- Modifications to allow the code to
;                                   interface with the
;                                   distance-omnibus repository, and
;                                   code cleanup for speed.
;       Modified: 01/06/12, TPEB -- Set tougher limits on max size of
;                                   Gaussian fit (previously had been
;                                   removing diffuse emission as
;                                   stars), and saving more info to
;                                   IDL save files.
;       Modified: 01/12/12, TPEB -- Added cutsie thing to estimate
;                                   when routine will be done with the
;                                   current GLIMPSE file.  Also,
;                                   trying to clear bug yielding
;                                   "PARINFO parameter limits are not
;                                   consistent" error.
;       Modified: 03/01/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.
;
;-

PRO OMNI_GLIMPSE_STARSUB, START=start, REAR=rear, FWHM=fwhm, BDR=bdr, $
                          TEST=test
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Define constant
  sfw = sqrt(8.d * alog(2.d))
  
  ;; Parse optional inputs
  test = KEYWORD_SET(test)
  IF n_elements(fwhm) EQ 0 THEN fwhm = 2.0 ; In arcsec of Band 4 PRF
  pix_prf = (fwhm/1.2d) / sfw              ; PRF sigma in pixels
  IF n_elements(bdr) EQ 0 THEN bdr = 9     ; Size of box around each star (15)
  
  IF test THEN BEGIN
     window,2,xsize=1600
     cgLoadct,3,/silent
  ENDIF
  
  ;; Find all Band 1 and Band 4 images based on information in the
  ;;   local-layout TPYE configuration file.  Check for errors.
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN
  ENDIF
  
  ;; Get file lists
  readcol,local.glimpse,glimpse,format='a',count=n_gl,/SILENT,comment='#'
  g4i = where(strmatch(glimpse,'*I4*',/fold),n4)
  g1i = where(strmatch(glimpse,'*I1*',/fold),n1)
  IF (n4 EQ 0) || (n1 EQ 0)  THEN BEGIN
     message,'Error: File '+local.glimpse+' does not contain IRAC Band 4 '+$
             'and/or Band 1 GLIMPSE images.  Exiting.',/cont
     RETURN
  ENDIF
  IF (n4 NE n1) THEN BEGIN
     message,'Error: Mismatch between Band 1 and Band 4 lists.  Exiting.',/cont
     RETURN
  ENDIF
  
  fn4 = glimpse[g4i]
  fn1 = glimpse[g1i]
  GDIR = strmid(fn4[0],0,strpos(fn4[0],'/',/reverse_search)+1)
  
  ;; Sort filename lists, just to be sure everything lines up nice.
  fn4 = fn4[sort(fn4)]
  fn1 = fn1[sort(fn1)]
  
  message,'Found '+string(n4,format="(I0)")+' GLIMPSE images.',/inf
  
  ;; Set start / rear index elements
  start = (n_elements(start) NE 0) ? long(start) - 1 : 0L
  rear  = (n_elements(rear) NE 0)  ? long(rear) - 1  : n4-1
  
  nloops = rear-start+1
  iloop = 0
  
  ;; Loop through the GLIMPSE images
  FOR p=start, rear DO BEGIN
     iloop += 1
     
     ;; Read in the 2 images
     i1img = readfits(fn1[p], i1hdr) > 0.d
     i4img = readfits(fn4[p], i4hdr) > 0.d
     
     ;; Align the two images to the Band 4 values
     hastrom,i1img,i1hdr,i4hdr
     imsize = size(i4img, /DIM)
     
     
     ;; Use IDLASTRO routine FIND.pro to find all the stars in the
     ;;   Band 1 image.  We use 20 as the theshold intensity for point
     ;;   sources, and 4 is the FWHM to be used in the colvolution.
     find_hmin = 20.0
     find_fwhm = 4.0
     
     find,i1img,x,y,flux,sharp,round,find_hmin,find_fwhm,$
          [-0.6,0.6],[0.05,1.50],/silent 
     
     ;; Clear out the memory of the Band 1 image...
     undefine,i1img
     
     ;; SORT the stars in decreasing order of FLUX, to remove the
     ;; brightest stars first.
     revind = reverse(sort(flux))
     flux   = flux[revind]
     x      = x[revind]
     y      = y[revind]
     
     
     ;; Once FIND has found all our stars, we will fit each of them
     ;;   with a 2-D Gaussian, as given by the routine BETTERFIT.pro.
     ;; Define the parinfo structure to be used with MPFIT
     parinfo = replicate( {value:0.d, limited:[1b,1b],limits:[0.d,0.d],$
                           maxstep:0.d,relstep:0.01d}, 6)
     
     ;; Sky baseline
     parinfo[0].limited = [0b,0b]
     
     ;; Gaussian widths (x & y)
     parinfo[2:3].value  = pix_prf
     parinfo[2:3].limits = [0.6,2.5]*pix_prf
     
     ;; Limit the maximum step per iteration for centriods and widths
     parinfo[2:5].maxstep = 0.01d
     
     ;; For each star found above, pull out a little stamp around the
     ;; x and y coordinate,and model it with a gaussian (mpfit2dpeak),
     ;; get parameters of gaussian - subtract gaussian from that
     ;; section of the i1img, then replace values with constant
     ;; baseline value (parameters[0]) 
     nx = n_elements(x)
     
     undefine,star_fwhm
     star_fwhm = replicate({fwx:0., fwy:0., x:0L,y:0L}, nx)
     start_t = SYSTIME(1)
     
     message,'Starting to subtract stars from '+fn4[p]+'...',/inf
     FOR i=0L,nx-1 DO BEGIN
        ellapsed = SYSTIME(1) - start_t
        frac = double(i+1)/double(nx)
        time_str = strmid(SYSTIME(0, start_t + ellapsed/frac),4,12)
        
        ;; Print status only every 1000 stars...
        IF (i+1) MOD 1000 EQ 0 THEN $
           print,string(i+1,nx,frac*100.,iloop,nloops,time_str,format=$
                        "('Subrtracting star #',I7,' of ',I7,'   ('"+$
                        ",F5.1,'%)  [',I2,' of ',I2,']  ',A0)")
        
        ;; These will all be boundary conditions so we do not get
        ;;   subscript errors and whatnot 
        stmpsz1 = (x[i]-bdr) > 0
        stmpsz2 = (x[i]+bdr) < (imsize[0]-1)
        stmpsz3 = (y[i]-bdr) > 0
        stmpsz4 = (y[i]+bdr) < (imsize[1]-1)
        
        undefine,stamp
        stamp = i4img[stmpsz1:stmpsz2,stmpsz3:stmpsz4]
        stmpsz = size(stamp,/DIM)
        
        ;; Memory cleanup
        undefine,xarray,yarray
        xarray = (findgen(stmpsz[0]))#(replicate(1.,stmpsz[1]))
        yarray = (replicate(1.,stmpsz[0]))#(findgen(stmpsz[1]))
        
        ;; Load best guess into parinfo.value
        mmm,stamp,mode,skysig,/SILENT
        ;; If unable to determine mode of image, use median
        IF skysig EQ -1. THEN mode = median(stamp)
        
        parinfo[0].value = mode
        parinfo[4].value = x[i] - stmpsz1
        parinfo[5].value = y[i] - stmpsz3
        parinfo[1].value = (stamp[parinfo[4].value,parinfo[5].value]) > 0
        
        ;; Set limits based on individual star
        parinfo[1].limits[1] = max(stamp,/NAN) ;i4img[x[i],y[i]]*(1.0)
        parinfo[4].limits    = [parinfo[4].value-1,parinfo[4].value+1]
        parinfo[5].limits    = [parinfo[5].value-1,parinfo[5].value+1]
        
        ;; Check to see if STAMP is out of Band 4 coverage...
        IF parinfo[1].limits[1] NE 0. THEN BEGIN
           
           ;;========================================
           ;; Run MPFIT2DFUN...
           ;;========================================
           undefine,parameters,z
           parameters = mpfit2dfun('betterfit',xarray,yarray,stamp,$
                                   parinfo.value,PARINFO=parinfo,$
                                   YFIT=z,/QUIET,STATUS=status,ERRMSG=errmsg,$
                                   WEIGHTS=1.d,NITER=niter)
           
           ;; If error, set to FAIL-SAFE
           IF status EQ 0 THEN BEGIN
              message,errmsg,/cont
              z = stamp * 0.
              parameters[0] = 0.
           ENDIF ELSE BEGIN
              IF test THEN BEGIN
                 print,parinfo[2].limits,parameters[2:3]
                 erase & multiplot,[3,1]
                 pr = [0,max(stamp)]
                 shade_surf,stamp,zrange=pr,/zst,title='Star'
                 multiplot
                 shade_surf,z,zrange=pr,/zst,title='Model'
                 multiplot
                 shade_surf,stamp - (z-parameters[0]),zrange=pr,/zst,$
                            title='Residual'
                 multiplot,/reset
                 wait,1
              ENDIF
           ENDELSE
           
           star_fwhm[i].fwx = parameters[2]
           star_fwhm[i].fwy = parameters[3]
           star_fwhm[i].x   = x[i]
           star_fwhm[i].y   = y[i]
           
           ;; Remove fitted star from larger image
           i4img[stmpsz1:stmpsz2,stmpsz3:stmpsz4] = stamp - (z - parameters[0])
           
        ENDIF                   ; End of star fit / removal IF block
        
     ENDFOR                     ; End of star-counting loop
     
     
     ;; Come up with filename to write to
     sfn = strmid(fn4[p],strpos(fn4[p],'/',/REVERSE_SEARCH))
     stringarray = strsplit(sfn,'_',/extract)
     coordinate  = strmid(stringarray[1],0,5)
     outfn = 'starsub.'+coordinate+'.'+stringarray[3]
     ;; save,star_fwhm,filename='irdc_dist_model/data/star_fwhm.'+coordinate+$
     ;;      '.sav',/ver
     
     ;; Add header history to describe the star subrtraction process
     hist = ['OMNI_GLIMPSE_STARSUB: '+systime(0),$
             'Star-subtracted GLIMPSE / IRAC Band 4 image']
     
     sxaddhist, '==============================', i4hdr, /COMMENT
     sxaddhist, hist, i4hdr
     sxaddpar, i4hdr, 'H_MIN', find_hmin, $
               ' Threshold hmin value used in FIND.pro', FORMAT="F0.1"
     sxaddpar, i4hdr, 'STARBDR', bdr, $
               ' Border (box) size radius around star for fit',FORMAT="I0"
     sxaddpar, i4hdr, 'NSUBTR', nx, $
               ' Number of stars subtracted from this image'
     sxaddpar, i4hdr, 'ORIGFILE', sxpar(i4hdr, 'FILENAME'), $
               ' Original GLIMPSE image filename'
     sxaddpar, i4hdr, 'FILENAME', outfn
     
     ;; Write the star-subtracted image to file, clipping negative values
     writefits, GDIR+outfn, i4img>0.d, i4hdr
     
     ;; Clean up memory
     undefine,i4img
     
  ENDFOR
  
  ;;======================================
  ;; End of Code
  print,'End Time:' & spawn,'date'
  
END
