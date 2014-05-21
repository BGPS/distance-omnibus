;+
; NAME:
;       OMNI_GLIMPSE_EMAF
;
; PURPOSE:
;       Create the specialized postage stamp images from the GLIMPSE
;       mosaics used for the EMAF morphological matching.
;
; CATEGORY:
;       distance-omnibus Local File Creation (EMAF-specific)
;
; CALLING SEQUENCE:
;       OMNI_GLIMPSE_EMAF [,TONS OF DEBUGGING OPTIONS]
;
; INPUTS:
;       NONE (all inputs hardwired)
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;
;       CNUM_LIST -- List of BGPS catalog numbers to process.
;                    [Default: ALL]
;       IFACT     -- Factor multiplying MAD(irac) for histogram width
;                    about MODE(irac) to use for fitting I_MIR
;                    surface.
;
; KEYWORD PARAMETERS:
;       ADD_SUFF  -- Add the appropriate suffix to output filenames to
;                    identify the type of fit / size used.
;       FITLINEAR -- Fit a planar surface to the I_MIR rather than a
;                    quadratic.
;       VERBOSE   -- Be verbose.
;       TEST      -- Do certain testing procedures.
;
; OUTPUTS:
;       NONE (postage stamp images written to file)
;
; OPTIONAL OUTPUTS:
;       IMIR_STATS -- Statistics related to the I_MIR fitting.
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; NOTES:
;       Requires the running of OMNI_ASSOC_CATALOG.pro -- checks for
;       the presence of the output from that routine and runs it, if
;       necessary.
;
; MODIFICATION HISTORY:
;
;       Created:  xx/xx/xx, Steve Mairs -- Initial verision, called
;                                          irdarkclouds2.pro in code
;                                          retrieved from
;                                          signals.siglab.ok.ubc.ca
;       Modified: 12/07/11, TPEB -- Modifications to allow the code to
;                                   interface with the
;                                   distance-omnibus repository, and
;                                   code cleanup for speed.
;       Modified: 12/21/11, TPEB -- Name change, and move within
;                                   repository structure.
;       Modified: 01/03/12, TPEB -- Point to new starsub files created
;                                   by process_glimpse_starsub.pro,
;                                   and other cleanup.
;       Modified: 01/11/12, TPEB -- Removed any use of the IRAC Band 1
;                                   image, which is irrelevant to this
;                                   routine.
;       Modified: 01/12/12, TPEB -- Fixed (simplified) method of
;                                   choosing which GLIMPSE or BGPS
;                                   mosaic to use, conforming with
;                                   remainder of distance-omnibus
;                                   repository.
;       Modified: 02/10/12. TPEB -- Adjusting the algorithm for
;                                   fitting the plane for I_MIR,
;                                   original code from Steve selected
;                                   pixels within 1x MAD of the mode,
;                                   but text from Erik says to use
;                                   1.47x.  Additional code changes
;                                   for testing purposes.
;       Modified: 03/26/12, TPEB -- Added new output file for the
;                                   label map.  This ensures that the
;                                   labels for morphological matching
;                                   are properly registered with the
;                                   bgps and glimpse images created
;                                   here.
;       Modified: 04/23/12, TPEB -- Added capability to fit a
;                                   quadratic surface to the I_MIR
;                                   rather than a plane.  This will be
;                                   useful for larger postage stamps.
;       Modified: 05/07/12, TPEB -- Added capacity to label filename
;                                   for postage-stamp size and type of
;                                   fit to IMIR.
;       Modified: 07/05/12, TPEB -- Modified default behavior to be
;                                   "_Q8".
;       Modified: 07/06/12, TPEB -- Updated documentation.
;       Modified: 08/07/12, TPEB -- Dealt with the 2 sources who fall
;                                   off the bottom of the BGPS mosaic.
;       Modified: 10/19/12, TPEB -- Added IRAC Band 4 scattering term
;                                   correction here so that downstream
;                                   postage stamp images may be
;                                   treated as correct.
;       Modified: 10/22/12, TPEB -- Set default mode to _q6, since the
;                                   IRAC camera FOV is 5.2' x 5.2'.
;                                   The new default size means the
;                                   scattering correction is closer to
;                                   the ideal per-IRAC-frame
;                                   correction that should have been
;                                   done within the GLIMPSE pipeline.
;                                   Also, reduced size of psf image
;                                   used to compute smoothed images.
;                                   New psf covers 10 orders of
;                                   magnitude, but no more.
;       Modified: 01/08/13, TPEB -- Changed smoothing kernel to
;                                   33" instead of 30" (BGPS group
;                                   groussing about 30").
;       Modified: 01/23/13, TPEB -- Added FITS keywords for Galactic
;                                   coordinates of the object in
;                                   question, and the estimate of the
;                                   mode from MMM.pro.
;       Modified: 01/24/13, TPEB -- Changed method for determining
;                                   "background pixels" for the I_MIR
;                                   surface fitting business.  Now
;                                   creating pixel histogram of ndata8
;                                   and selecting pixels within the
;                                   FWHM of the main peak.  In
;                                   general, this decreases the number
;                                   of pixels used in the background
;                                   fit, but the intensity range is
;                                   more closely suited to the
;                                   purpose, especially for
;                                   pathological cases.  Also added
;                                   FITS keywords related thereto.
;       Modified: 03/01/13, TPEB -- In the shift to OMNI_*.pro code
;                                   and generalized configuration file
;                                   input: name change and made
;                                   compatible with the new
;                                   framework.
;       Modified: 03/14/13, TPEB -- Added check for conf.haslabel,
;                                   since if 0b, then don't run
;                                   this routine since it will just
;                                   waste CPU cycles as PROB_EMAF
;                                   cannot be computed without them.
;                                   Also, adjusted computation of
;                                   label maps to be more generalized
;                                   than V1 was.
;       Modified: 08/02/13, TPEB -- Fixed minor bug with ARRAY[1] <-->
;                                   SCALAR non-identicality
;                                   w.r.t. SXADDPAR.
;       Modified: 08/21/13, TPEB -- I really hate the ARRAY[1] <-->
;                                   SCALAR non-identicality!!!  Fixed
;                                   bug there, and added OBJNUM FITS
;                                   keyword, which includes survey
;                                   name and catalog number.
;       Modified: 08/26/13, TPEB -- Using LABVAL from the
;                                   map_locations save file instead of
;                                   looking it up again; this fixes
;                                   some strange problem with the
;                                   label images.
;       Modified: 10/22/13, TPEB -- When using with ATLASGAL, found
;                                   various issues when sources have
;                                   |b| > 0.5 deg.  Bug fixes!
;
;-

PRO OMNI_GLIMPSE_EMAF, CONFFILE=cfile, CNUM_LIST=cnum_list, $
                       VERBOSE=verbose, ADD_SUFF=add_suff, FITLINEAR=fitlinear,$
                       TEST=test, IFACT=ifact, IMIR_STATS=imir_stats
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Parse keywords
  verbose  = KEYWORD_SET(verbose)
  silent   = ~verbose
  test     = KEYWORD_SET(test)
  fitquad  = ~KEYWORD_SET(fitlinear)
  no_suff  = ~KEYWORD_SET(add_suff)
  ifact = (n_elements(ifact) EQ 0) ? 1.47d : double(ifact)
  
  ;; Load relevant configuration files
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN
  ENDIF
  IF ~exist(local) THEN $
     local = omni_read_conffile('./conffiles/local_layout.conf')
  IF local.error THEN BEGIN
     message,local.error,/cont
     RETURN
  ENDIF
  
  ;; Make sure label maps exist -- if not, PROB_EMAF cannot be
  ;;   computed.  Check now, to save CPU time on computing these
  ;;   postage-stamp images.
  IF ~conf.haslabel THEN BEGIN
     message,'Error: Label maps for '+conf.survey+' do not exist!  '+$
             'OMNI_GLIMPSE_EMAF will not run, and PROB_EMAF will not be '+$
             'be computed!',/cont
     RETURN
  ENDIF
  
  ;; Parse optional input for size of boxes to extract around each
  ;;   source, and use default if not specified.  The code below
  ;;   ensures ps_size is an allowed value, and number of pixels and
  ;;   actual cutsize (in arcmin) will be computed after images are
  ;;   read in.
  IF fitquad THEN BEGIN
     imirtype='Quadratic'
     suff = '_q'
  ENDIF ELSE BEGIN
     imirtype='Plane'
     suff = '_l'
  ENDELSE
  
  ;; PS_SIZE must be an integer number of arcminutes between 4' and 8'.
  ps_size = fix(round(local.gpssize))
  IF ps_size LT 4 || ps_size GT 8 THEN BEGIN
     message,'Error: Unrecognized PS_SIZE value '+$
             string(ps_size,format="(I0)"),/cont
     RETURN
  ENDIF
  suff += string(ps_size,format="(I0)")
  IF no_suff THEN suff = ''
  
  ;; Begin by making a directory to place the output files
  OUTDIR = local.emafpost
  IF ~FILE_TEST(OUTDIR,/DIRECTORY) THEN spawn,'mkdir '+OUTDIR
  
  ;; Read in SURVEY structure & parse into simple variables
  s   = omni_read_cat(conf.cat,ncat,fmt)
  num = s.cnum
  l   = s.glon
  b   = s.glat
  fmt2 = string(ceil(alog10(ncat+1)),format="('I',I0)")
  
  ;; Read in IDL save file with SURVEY map positions -- check for
  ;;   presence, if no exist, then run OMNI_ASSOC_CATALOG.pro
  mapfn = './local/'+conf.survey+'_map_locations.sav'
  IF ~FILE_TEST(mapfn,/READ) THEN omni_assoc_catalog,CONFFILE=cfile
  restore,mapfn,/ver
  
  
  ;; Check for CNUM_LIST input
  IF n_elements( cnum_list ) NE 0 THEN BEGIN
     cind = WHERE_ARRAY(cnum_list, num, ncat)
     IF ncat EQ 0 THEN BEGIN
        message,'Error: CNUM_LIST not matched to SURVEY catalog',/cont
        RETURN
     ENDIF
     s      = s[cind]
     num    = num[cind]
     l      = l[cind]
     b      = b[cind]
     survey = survey[cind]
  ENDIF
  
  ;; Next, search for the Star-subtracted BAND 4 images, and SURVEY
  ;;   mosaics. 
  
  ;; Get BAND 1 Images
  readcol,local.glimpse,glimpse,format='a',count=n_gl,/SILENT,comment='#'
  g1i = where(strmatch(glimpse,'_misaic_I1.fits',/fold),nfn1)
  IF (nfn1 EQ 0)  THEN BEGIN
     message,'Error: File '+local.glimpse+' does not contain IRAC Band 1 '+$
             'GLIMPSE images.  Exiting.',/cont
     RETURN
  ENDIF
  fn1 = glimpse[g1i]
  
  ;; Get star-subtracted BAND 4 Images
  GDIR = strmid(fn1[0],0,strpos(fn1[0],'/',/reverse_search)+1)
  fn4 = FILE_SEARCH(GDIR+'starsub*.fits',            COUNT = nfn4)
  
  IF (nfn4 NE nfn1) THEN BEGIN
     message,'Error:  Mismatch between BAND 1 images and '+$
             'STARSUB images.  May need to run '+$
             'omni_glimpse_starsub.pro.  Exiting',/cont
     RETURN
  ENDIF
  
  ;; Sort filename lists, just to be sure everything lines up nice.
  fn4 = fn4[sort(fn4)]
  fn1 = fn1[sort(fn1)]
  
  ;; Find all the survey data mosaics, label maps, and smoothed images
  readcol,conf.maps,fnm,format='a',count=nfnm,/SILENT,comment='#'
  readcol,conf.label,fnl,format='a',count=nfnl,/SILENT,comment='#'
  IF conf.hassmooth THEN $
     readcol,conf.smooth,fns,format='a',count=nfns,/SILENT,comment='#' ELSE $
        nfns = 0
  
  
  ;; Read in FITS header for 1st SURVEY image, and first GLIMPSE image
  ;;   to establish pixel scales, postage-stamp cut sizes, smoothing
  ;;   kernel PSF size, etc.
  ghd = headfits(fn4[0])
  mhd = headfits(fnm[0])
  
  extast,ghd,gastr,gcd
  extast,mhd,mastr,mcd
  
  ;; Pixel scales in arcsec
  g_pixscl = (gcd EQ 1) ? abs(gastr.cdelt[0])*3600. : abs(gastr.cd[0,0])*3600.
  m_pixscl = (mcd EQ 1) ? abs(mastr.cdelt[0])*3600. : abs(mastr.cd[0,0])*3600.
  
  bdr  = long(round(float(ps_size*1.5)/2./g_pixscl*60. - 0.5)) ; GLIMPSE border
  bdr2 = long(round(float(ps_size*1.0)/2./m_pixscl*60. - 0.5)) ; SURVEY border
  
  cutsize = (2.*float(bdr2)+1.)*m_pixscl/60. ; Size of SURVEY postage stamp
  
  ;; Create SMOOTHING kernel for convolving GLIMPSE up to 
  ;;    SURVEY resolution  -- The PSF array is large enough to conver
  ;;    10 orders of magnitude of amplitude.
  ppfwhm = conf.fwhm/g_pixscl
  psfsize = REPLICATE( ppfwhm, 2)
  psf     = psf_gaussian(npixel=long(round([ppfwhm*5.64,ppfwhm*5.64])),$
                         fwhm=psfsize, /NORMALIZE)
  sfwhm = string(conf.fwhm,format="(I0)") ; String for filenames
  
  ;; Strings containing the name of the last file to be read in
  lastfn4 = ''
  lastfnm = ''
  
  ;; There is timing built in to estimate how long until completion
  start_t = SYSTIME(1)
  
  ;;===================================================================
  ;; Start looping through the objects!
  FOR p=0L, ncat-1 DO BEGIN
     ;;FOR p=0L,10 DO BEGIN
     
     ;; Print status of progress through the catalog...
     ellapsed = SYSTIME(1) - start_t
     frac = double(p+1)/double(ncat)
     time_str = strmid(SYSTIME(0, start_t + ellapsed/frac),4,12)
     print,string(s[p].cnum,ncat,float(p+1)/float(ncat)*100.,time_str,format=$
                  "('Running catalog object #',"+fmt2+$
                  ",' / ',"+fmt2+",'   (',F5.1,'%)  ',A0)")
     time0 = systime(1)
     
     
     ;; Choose which GLIMPSE file to use
     glon = string( round(s[p].glon / 3.) * 3., format="(I03)")
     IF glon EQ '360' THEN glon = '000'
     index = where( strmatch(fn4, '*'+glon+'00*'), nind1)
     IF nind1 EQ 0 THEN CONTINUE
     thisfn4 = fn4[index]
     
     ;; Choose which SURVEY file to use
     IF survey[p].mapname EQ '' THEN CONTINUE ; No map match
     index2 = where( strmatch(fnm,'*'+survey[p].mapname), nind2)
     IF nind2 EQ 0 THEN CONTINUE
     thisfnm = fnm[index2]
     thisfnl = fnl[index2]
     
     ;; If out of range of GLIMPSE, skip this source
     IF verbose THEN print,'NINDs for IRAC & SURVEY: ',nind1,nind2
     
     
     ;;=================================================================
     ;; Optimization - trying to get the script to run faster, i.e. we
     ;;   do not need to read in a new fits file each time if it is
     ;;   indeed the same file
     
     IF thisfn4 NE lastfn4 THEN BEGIN
        message,'Reading in new GLIMPSE mosaic... '+thisfn4,/inf
        lastfn4 = thisfn4
        undefine,data4,adata4,hd4,ahd4,astrom4
        
        ;; Read in file & perform header astrometery (force FLOAT)
        data4 = TEMPORARY(float(TEMPORARY(readfits(thisfn4,hd4))))
        extast,hd4,astrom4
        
        ;; Muck with the FITS header
        sxaddpar,hd4,'BITPIX',-32
        sxaddhist, '==============================', hd4, /COMMENT
        hist = ['OMNI_GLIMPSE_EMAF: '+systime(0),$
                'Created IRDC postage stamp images']
        sxaddhist, hist, hd4
        sxaddpar, hd4, 'SSUBFILE', sxpar(hd4,'FILENAME'), $
                  ' Star-subtracted GLIMPSE image'
        sxaddpar, hd4, 'CUTSIZE', cutsize, ' [arcmin] Size of postage stamp', $
                  FORMAT="F0.2"
        sxdelpar, hd4, ['DELTA-X','DELTA-Y','BORDER','RA','DEC']
        FOR rmi = 1, 300 DO BEGIN
           sxdelpar, hd4, string(rmi,format="('AOR',I03)")
           sxdelpar, hd4, string(rmi,format="('DSID',I03)")
        ENDFOR
     ENDIF      
     
     ;; Now, do SURVEY image
     IF thisfnm NE lastfnm THEN BEGIN
        message,'Reading in new SURVEY mosaic... '+thisfnm,/inf
        lastfnm = thisfnm
        undefine,datab,hdb,astromb
        
        ;; Read in file & perform header astrometery  (force FLOAT)
        datab = TEMPORARY(float(readfits(thisfnm,hdb)))
        extast,hdb,astromb
        label = TEMPORARY(fix(readfits(fnl[index2],hdl)))
        extast,hdl,astroml
        IF conf.hassmooth THEN $
           datas = TEMPORARY(float(readfits(fns[index2],hds)))
        
        ;; Muck with the FITS header
        basenamem = (strmid(thisfnm,strpos(thisfnm,'/',/reverse_search)+1))[0]
        basenamel = (strmid(thisfnl,strpos(thisfnl,'/',/reverse_search)+1))[0]
        sxaddpar,hdb,'BITPIX',-32
        sxaddpar,hdb,'FILENAME',basenamem,' Name of this file',$
                 AFTER='WAVELENG'
        sxaddpar,hdl,'FILENAME',basenamel,' Name of this file',$
                 AFTER='WAVELENG'
        sxaddhist, '==============================', hdb, /COMMENT
        sxaddhist, '==============================', hdl, /COMMENT
        hist = ['OMNI_GLIMPSE_EMAF: '+systime(0),$
                'Created IRDC postage stamp images']
        sxaddhist, hist, hdb
        sxaddhist, hist, hdl
        sxaddpar,hdb,'ORIGFILE',sxpar(hdb,'FILENAME'),' Original SURVEY mosaic'
        sxaddpar,hdl,'ORIGFILE',sxpar(hdl,'FILENAME'),' Original LABEL map'
        sxaddpar, hdb, 'CUTSIZE', cutsize, ' [arcmin] Size of postage stamp', $
                  FORMAT="F0.2"
        sxaddpar, hdl, 'CUTSIZE', cutsize, ' [arcmin] Size of postage stamp', $
                  FORMAT="F0.2"
     ENDIF 
     
     ;; Perform header astrometry
     ad2xy,l[p],b[p],astrom4,x4,y4
     ad2xy,l[p],b[p],astromb,xb,yb
     ad2xy,l[p],b[p],astroml,xl,yl
     labval = survey[p].labval ; Label value for use in writing FITS
     
     ;; Define the boxes for postage stamp image extraction, rounding
     ;;   to nearest pixel
     sz4 = [astrom4.naxis[0],astrom4.naxis[1]]
     szb = [astromb.naxis[0],astromb.naxis[1]]
     szl = [astroml.naxis[0],astroml.naxis[1]]
     
     box       = long(round([(x4-bdr)>0,(x4+bdr)<(sz4[0]-1),$
                             (y4-bdr)>0,(y4+bdr)<(sz4[1]-1)]))
     surveybox = long(round([(xb-bdr2)>0,(xb+bdr2)<(szb[0]-1),$
                             (yb-bdr2)>0,(yb+bdr2)<(szb[1]-1)]))
     labelbox  = long(round([(xl-bdr2)>0,(xl+bdr2)<(szl[0]-1),$
                             (yl-bdr2)>0,(yl+bdr2)<(szl[1]-1)]))
     ;; Check for "out-of-bounds" boxes, or even boxes 1/2 as large as
     ;;   they should be due to proximity to image edge.
     IF box[3] - box[2] LE bdr || $
        box[1] - box[0] LE bdr THEN CONTINUE
     IF surveybox[3] - surveybox[2] LE bdr2 || $
        surveybox[1] - surveybox[0] LE bdr2 THEN CONTINUE
     IF labelbox[3] - labelbox[2] LE bdr2 || $
        labelbox[1] - labelbox[0] LE bdr2 THEN CONTINUE
     
     ;; Go about extracting the proper sections of the images.
     hextract,data4,hd4,ndata4,nhd4,box[0],box[1],box[2],box[3],SILENT=silent
     hextract,datab,hdb,surveystamp,nhdb,$
              surveybox[0],surveybox[1],surveybox[2],surveybox[3],SILENT=silent
     hextract,label,hdl,lblstamp,nhdl,$
              labelbox[0],labelbox[1],labelbox[2],labelbox[3],SILENT=silent
     IF conf.hassmooth THEN $
        hextract,datas,hds,smoostamp,smoostamphd,$
                 surveybox[0],surveybox[1],surveybox[2],surveybox[3],$
                 SILENT=silent
     
     ;; Add CNUM, GLON, & GLAT for this source to the FITS header
     sxaddpar,nhd4,'OBJNUM',conf.survey+' '+string(num[p],format='('+fmt2+')'),$
              ' Survey Object Identifier'
     sxaddpar,nhd4,'GLON',l[p],' [deg] Galactic Longitude for this object',$
              FORMAT="F0.4"
     sxaddpar,nhd4,'GLAT',b[p],' [deg] Galactic Latitude for this object',$
              FORMAT="F0.4"
     sxaddpar,nhdb,'OBJNUM',conf.survey+' '+string(num[p],format='('+fmt2+')'),$
              ' Survey Object Identifier'
     sxaddpar,nhdb,'GLON',l[p],' [deg] Galactic Longitude for this object',$
              FORMAT="F0.4"
     sxaddpar,nhdb,'GLAT',b[p],' [deg] Galactic Latitude for this object',$
              FORMAT="F0.4"
     sxaddpar,nhdl,'OBJNUM',conf.survey+' '+string(num[p],format='('+fmt2+')'),$
              ' Survey Object Identifier'
     sxaddpar,nhdl,'GLON',l[p],' [deg] Galactic Longitude for this object',$
              FORMAT="F0.4"
     sxaddpar,nhdl,'GLAT',b[p],' [deg] Galactic Latitude for this object',$
              FORMAT="F0.4"
     
     ;; Check on the 2 sources falling off the bottom of a SURVEY mosaic
     IF (yb-bdr2) LT 0 THEN BEGIN
        below = (long(round(abs(yb-bdr2))))[0] ; Force scalar
        newstmp = fltarr(2*bdr2+1,2*bdr2+1)
        newstmp[*,below:*] = surveystamp
        sxaddpar,nhdb,'NAXIS2',2*bdr2+1,format="(I0)"
        sxaddpar,nhdb,'CRPIX2',sxpar(nhdb,'CRPIX2')+below
        surveystamp = TEMPORARY(newstmp)
     ENDIF
     
     ;;================================================================
     ;; Section of code dealing with fitting the I_MIR to the cutout
     
     boxsz = size(ndata4,/DIM)
     xarr = cmreplicate(findgen(boxsz[0]),boxsz[1]) ; The column array
     yarr = transpose(cmreplicate(findgen(boxsz[1]),boxsz[0])) ; The row array
     
     ;; Estimate the sky background in a stellar contaminated field.
     mmm,ndata4,mo,skysig,/SILENT
     sxaddpar,nhd4,'SKYMODE',mo,' [MJy/sr] Mode of image from MMM.pro',$
              FORMAT="F0.3"
     sxaddpar,nhd4,'SKYSIG',skysig,FORMAT="F0.3",$
              ' [MJy/sr] Sky sigma from MMM.pro (-1 = FAIL)'
     IF skysig EQ -1 THEN BEGIN
        ;; I guess, at one time, I thought the program should do
        ;;   something here?
        
     ENDIF
     xcen = (box[1]-box[0])/2
     ycen = (box[3]-box[2])/2
     
     ;; Maybe need to divide second term in where function by 2? --
     ;;   IFACT = 1.47 from paragraph Erik sent for IRDC paper
     index = where(abs(ndata4-mo) LT mad(ndata4)*ifact)
     
     ;;============
     ;; Alternative version: Make pixel histogram of HASTROM'd
     ;;   image with 1 MJy/sr bins, and take all pixels from
     ;;   the 1.2"-resolution image w/in FWHM
     histbin = 1.0
     hastrom,ndata4,nhd4,test4,thd4,nhdb,missing=0.
     ;; Check if we're off the edge of the GLIMPSE coverage,
     ;; but still on the FITS image...CONTINUE (i.e. skip this one).
     IF max(test4) EQ min(test4) THEN CONTINUE
     plothist,/noplot,test4,Iarr,Narr,bin=histbin
     ihist = where(Narr GE 0.5*max(Narr))
     fitr = minmax(Iarr[ihist]) + ([-1.,1.]*histbin/2.)
     index = where(ndata4 GE fitr[0] AND ndata4 LE fitr[1], nindex)
     sxaddpar,nhd4,'IFIT_MIN',fitr[0],FORMAT="F0.3",$
              ' [MJy/sr] Minimum pix value used for I_MIR fit'
     sxaddpar,nhd4,'IFIT_MAX',fitr[1],FORMAT="F0.3",$
              ' [MJy/sr] Maximum pix value used for I_MIR fit'
     sxaddpar,nhd4,'IFIT_NPIX',nindex,' Number of pixels used for the I_MIR fit'
     ;;============
     
     IF test THEN BEGIN
        mask        = ndata4*0.
        mask[index] = 1.
        plotmap,mask,nhd4,title='IFACT = '+string(ifact,format="(F0.1)")
     ENDIF
     
     ;;=================================================================
     ;; Perform the **LEAST SQUARES FIT**
     ;; mat ## fitvec = RHS
     
     ;; (x,y,z) coordinates of the pixels used for the fit     
     xp = xarr[index]-xcen
     yp = yarr[index]-ycen
     zp = ndata4[index]
     
     ;; Calculate the TERMS used for the matrix
     nterm = fitquad ? 6 : 3
     mat = dblarr(nterm,nterm)
     
     mat[0,0] = n_elements(xp)
     mat[1,0] = total(xp)
     mat[2,0] = total(yp)
     mat[1,1] = total(xp*xp)
     mat[2,1] = total(xp*yp)
     mat[2,2] = total(yp*yp)
     IF fitquad THEN BEGIN
        mat[3,0] = total(xp*xp)
        mat[4,0] = total(yp*yp)
        mat[5,0] = total(xp*yp)
        mat[3,1] = total(xp*xp*xp)
        mat[4,1] = total(xp*yp*yp)
        mat[5,1] = total(xp*xp*yp)
        mat[3,2] = total(xp*xp*yp)
        mat[4,2] = total(yp*yp*yp)
        mat[5,2] = total(xp*yp*yp)
        mat[3,3] = total(xp*xp*xp*xp)
        mat[4,3] = total(xp*xp*yp*yp)
        mat[5,3] = total(xp*xp*xp*yp)
        mat[4,4] = total(yp*yp*yp*yp)
        mat[5,4] = total(xp*yp*yp*yp)
        mat[5,5] = total(xp*xp*yp*yp)
     ENDIF
     
     ;; Fill in the symmetric matrix
     FOR ii=0,nterm-1 DO mat[ii,*] = mat[*,ii]
     
     ;; The right hand side of the matrix equation
     RHS    = dblarr(1,nterm)
     RHS[0] = total(zp)
     RHS[1] = total(xp*zp)
     RHS[2] = total(yp*zp)
     IF fitquad THEN BEGIN
        RHS[3] = total(xp*xp*zp)
        RHS[4] = total(yp*yp*zp)
        RHS[5] = total(xp*yp*zp)
     ENDIF    
     
     ;; Calculate the fit vector
     fitvec = (invert(mat))##RHS
     
     ;; Calculate I_MIR
     Imir = fitvec[0] + fitvec[1]*(xarr-xcen) + fitvec[2]*(yarr-ycen)
     IF fitquad THEN $
        Imir += fitvec[3]*(xarr-xcen)*(xarr-xcen) + $
                fitvec[4]*(yarr-ycen)*(yarr-ycen) + $
                fitvec[5]*(xarr-xcen)*(yarr-ycen)
     
     IF test THEN $
        imir_stats = m4_stat(Imir)
     
     ;; Clear memory -- Cleaner in the code this way than tons of
     ;;                 TEMPORARY() statements.
     undefine,index,xp,yp,zp,xarr,yarr
     
     
     ;;============================================================
     ;; IRAC Band 4 suffers from pretty bad scattered light within the
     ;; camera.  No light is lost, but it is internally reflected all
     ;; over the place.  Therefore, there is a photometric aperture
     ;; correction that must be applied to surface brightness
     ;; measurements.  For extended diffuse emission (read: Galactic
     ;; Plane), this correction factor is 0.737, meaning the true
     ;; surface brightness is only 73.7% of that in the GLIMPSE
     ;; mosaics.  Furthermore, for IRDCs the dark region is filled
     ;; with scattered BRIGHT emission.  The propagation of this
     ;; correction leads to the TRUE CONTRAST = C_meas(1 + \xi), where
     ;; \xi is 1 - correction, or 0.263.
     xi = 0.263
     
     ;; We deal with the scattering here by subtracting X = xi * IMIR
     ;; from both the IMIR fit (above) and the ndata4 cutout.  Add
     ;; value of X to the FITS headers.
     
     ;; Xscat is xi times the mean of IMIR rather than a
     ;; position-dependent array:
     Xscat = xi * mean(Imir)
     
     Imir -= Xscat
     ndata4 -= Xscat
     
     sxaddpar,nhd4,'X_SCAT',Xscat,$
              ' [MJy/sr] Scattered intensity subtracted',FORMAT="F0.3"
     
     
     ;;================================================================
     ;; Section of code dealing with smoothing
     
     ;; Median smooth regular Band 4 image to remove effects of
     ;;    badly-subtracted stars
     ndata4 = median(ndata4 > 0., 5)
     
     ;; Make smoothed image
     smooth30 = float(convolve(ndata4,psf,FT_PSF=psf_ft,/NO_PAD) > 0.)
     
     ;; print,'After convolve:  ',systime(1) - time0
     
     ;;================================================================
     ;; Start the great FITS-writing!
     ;; NOTE: resampling up to SURVEY pixel scale done at this point!!!
     scnum = string(num[p],format=fmt)
     
     ;; Non-smoothed image
     hastrom,ndata4,nhd4,ndata4,svhd4,nhdb,missing=0.
     sxaddpar,svhd4,'PIXSCAL1',abs(sxpar(svhd4,'CD1_1'))*3600.,format="F0.3"
     sxaddpar,svhd4,'PIXSCAL2',abs(sxpar(svhd4,'CD2_2'))*3600.,format="F0.3"
     wfn = conf.survey+'_nonsmo'+scnum+suff+'.fits'
     sxaddpar,svhd4,'FILENAME',wfn
     writefits,OUTDIR+wfn,TEMPORARY(float(TEMPORARY(ndata4))),TEMPORARY(svhd4)
     
     ;; Smoothed image
     hastrom,smooth30,nhd4,smooth30,svhd4,nhdb,missing=0.
     sxaddpar,svhd4,'PIXSCAL1',abs(sxpar(svhd4,'CD1_1'))*3600.,format="F0.3"
     sxaddpar,svhd4,'PIXSCAL2',abs(sxpar(svhd4,'CD2_2'))*3600.,format="F0.3"
     wfn = conf.survey+'_s'+sfwhm+'arc'+scnum+suff+'.fits'
     sxaddpar,svhd4,'FILENAME',wfn
     writefits,OUTDIR+wfn,TEMPORARY(float(TEMPORARY(smooth30))),TEMPORARY(svhd4)
     
     ;; Imir image
     hastrom,Imir,nhd4,Imir,svhd4,nhdb,missing=0.
     sxaddpar,svhd4,'PIXSCAL1',abs(sxpar(svhd4,'CD1_1'))*3600.,format="F0.3"
     sxaddpar,svhd4,'PIXSCAL2',abs(sxpar(svhd4,'CD2_2'))*3600.,format="F0.3"
     wfn = conf.survey+'_Imir'+scnum+suff+'.fits'
     sxaddpar,svhd4,'FILENAME',wfn
     sxaddpar,svhd4,'IFACT',ifact,' Times MAD around mode to fit IMIR',$
              FORMAT="F0.2"
     sxaddpar,svhd4,'IMIRTYPE',imirtype,' Type of fit to I_MIR'
     writefits,OUTDIR+wfn,TEMPORARY(float(TEMPORARY(Imir))),TEMPORARY(svhd4)
     undefine,nhd4
     
     ;; SURVEY label
     hastrom,lblstamp,nhdl,nhdb,missing=0.
     wfn = conf.survey+'_label'+scnum+suff+'.fits'
     sxaddpar,nhdl,'FILENAME',wfn
     sxaddpar,nhdl,'BITPIX',8
     sxaddpar,nhdl,'LABVAL',labval,' Value of the label mask for this object'
     writefits,OUTDIR+wfn,TEMPORARY(byte(TEMPORARY(lblstamp) EQ labval)),$
               TEMPORARY(nhdl)
     
     IF conf.hassmooth THEN BEGIN
        ;; SURVEY smoothed
        hastrom,smoostamp,smoostamphd,nhdb,missing=0.
        wfn = conf.survey+'_datsmo'+scnum+suff+'.fits'
        sxaddpar,smoostamphd,'FILENAME',wfn
        writefits,OUTDIR+wfn,TEMPORARY(float(TEMPORARY(smoostamp))),$
                  TEMPORARY(smoostamphd)
     ENDIF
     
     ;; SURVEY image
     wfn = conf.survey+'_mapdat'+scnum+suff+'.fits'
     sxaddpar,nhdb,'FILENAME',wfn
     writefits,OUTDIR+wfn,TEMPORARY(float(TEMPORARY(surveystamp))),$
               TEMPORARY(nhdb)
     
  ENDFOR   ;; END of source looping
  
  ;; Clear out memory
  undefine,data4,hd4,astrom4
  undefine,datab,hdb,astromb,label,hdl,datas,hds
  
  ;;======================================
  ;; End of Code
  print,'End Time:' & spawn,'date'
  
END
