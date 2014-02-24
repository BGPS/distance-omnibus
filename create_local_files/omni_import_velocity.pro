;+
; NAME:
;       OMNI_IMPORT_VELOCITY
;
; PURPOSE:
;       Read in the Machine-Readable Tables containing velocity
;       information from molecular line emission according to the
;       survey-info configuration file.  Creates an IDL save file in
;       the local/ directory.
;
; CATEGORY:
;       distance-omnibus Local File Creation
;
; CALLING SEQUENCE:
;       OMNI_IMPORT_VELOCITY [,CONFFILE=cfile]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;
; KEYWORD PARAMETERS:
;       VERBOSE   -- Cause the routine to contract logorrhoea.
;       NOGRSFILT -- Do not apply a SAVOL filter to the GRS 13CO
;                    spectra.
;
; OUTPUTS:
;       VEL -- Structure containing velocity information for each of
;              the catalog enties from the conffile.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;       GRS_BLOCK   -- Contains the GRS spectrum structure (created
;                      with generate_grs_spectra_local.pro).
;
; MODIFICATION HISTORY:
;
;       Created:  02/25/13, TPEB -- Initial version, melding of
;                                   create_velocity_structure.pro,
;                                   import_densegas_spectra.pro, and
;                                   validate_velocity.pro.
;       Modified: 02/28/13, TPEB -- Removed annoying print statement,
;                                   added status message; also, added
;                                   COMMON block for configuration
;                                   structures.
;       Modified: 03/05/13, TPEB -- Added means for encoding which
;                                   velocities are present via the
;                                   rt_types element.  Uses bitwise
;                                   operators based on the velocity
;                                   lists in survey_info.conf.
;       Modified: 03/07/13, TPEB -- Fixed bug in reading in catalogs
;                                   where non-detections are listed
;                                   with TMB < 0.
;       Modified: 03/08/13, TPEB -- Fixed bug in rv_types flag: now
;                                   starts with 1 for first spectral
;                                   line (2^0).
;       Modified: 07/31/13, TPEB -- Uses survey LABEL maps, if
;                                   available, for associating a
;                                   velocity pointing with catalog
;                                   objects.  Still uses nearest if
;                                   LABEL maps do not exist.  Checks
;                                   for multiple velocities in the
;                                   same object -- sets MULTIV flag if
;                                   contributing velocities differ by
;                                   > 7 km/s.
;       Modified: 08/05/13, TPEB -- Small bug fix with previous
;                                   additions.
;       Modified: 08/06/13, TPEB -- Changed MULTIV criteria to > 5
;                                   km/s, and extended checking and
;                                   combination criteria to the final
;                                   conglomerated velocity data.
;       Modified: 08/22/13, TPEB -- Added structure elements to hold
;                                   GRS 13CO extracted information,
;                                   following the criteria developed
;                                   at the Edmonton2013 working
;                                   group.
;       Modified: 08/26/13, TPEB -- Added flags for the GRS 13CO
;                                   velocities, and added a -1
;                                   RV_TYPES flag for conflicting
;                                   multiple dense gas velocities.
;       Modified: 09/06/13, TPEB -- Added structure element for GRS
;                                   linewidth.
;       Modified: 10/24/13, TPEB -- Allowed non-use of GRS 13CO if
;                                   specified in survey_info.conf.
;       Modified: 12/09/13, TPEB -- Yancy didn't check his
;                                   data, so I lost a day of work
;                                   tracking down the issue.  Added a
;                                   check for NON-FINITE linewidths
;                                   that makes everything downstream
;                                   play nice.
;       Modified: 12/17/13, TPEB -- More cleaning up after Yancy.
;       Modified: 12/18/13, TPEB -- Added means for returning ONSPEC
;                                   and flag for LANDLOCKED GRSMATCH
;                                   cases.  New GRS flags: [4, 5, 6]
;                                   for [single peak ONSPEC,
;                                   distinguishable ONSPEC,
;                                   indistinguishable ONSPEC].
;       Modified: 12/23/13, TPEB -- Conformal changes to allow /NOFILT
;                                   option for GRS SAVGOL filtering.
;
;-

PRO OMNI_IMPORT_VELOCITY, CONFFILE=cfile, VERBOSE=verbose, NOGRSFILT=nogrsfilt
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON GRS_BLOCK, grs, grsexist, v_std
  
  ;; Parse keywords
  silent    = ~KEYWORD_SET(verbose)
  nogrsfilt = KEYWORD_SET(nogrsfilt)

  ;; Read in the configuration file
  conf = omni_load_conf(cfile)
  IF conf.error THEN BEGIN
     message,conf.error,/cont
     RETURN
  ENDIF
  
  ;; Define the beam aperture based on SURVEY solid angle
  ap_size = double(ceil(sqrt(conf.omega/!dpi)*2.*206265.))
  
  ;; Check for presence of velocity information
  ind = where(conf.v.has EQ 1, nvel)
  IF nvel EQ 0 THEN BEGIN
     message,'This survey has no velocity data.  Exiting.',/cont
     RETURN
  END
  vel = conf.v[ind]
  
  ;; Read in the survey catalog & count entries, and also
  ;;   read in SURVEY structure
  s = omni_read_cat(conf.cat,ncat,fmt)
  restore,'./local/'+conf.survey+'_map_locations.sav',VERBOSE=~silent
  
  ;; If present, locate the label maps
  IF conf.haslabel THEN $
     readcol,conf.label,survey_label,format='a',count=n_map, $
             SILENT=silent,comment='#' ELSE n_map = 0
  IF ~n_map THEN conf.haslabel = 0b
  
  ;; If present, load label maps into memory
  IF conf.haslabel THEN BEGIN
     
     ;; Check for the cropping -- it's in here so we don't do it
     ;;   unless we have label maps,
     IF conf.hascrop THEN $
        readcol,conf.crop,fieldname,lmin,lmax,bmin,bmax,format='a,f,f,f,f',$
                count=n_crop,SILENT=silent,comment='#' ELSE n_crop = 0
     
     message,'Reading label maps into memory...',/inf
     mapdata = replicate( {fn:'',$
                           naxis:[0,0],$
                           l:[0.d,0.d],$
                           b:[0.d,0.d]}, n_map)
     
     ;; Loop over maps     
     FOR k=0, n_map-1 DO BEGIN
        kst = string(k,format="(I0)")
        command = 'label'+kst+' = readfits(survey_label[k],lhd'+kst+$
                  ',SILENT=silent)'
        errcode = Execute(command)
        
        ;; Create an ASTR structure
        command = 'extast,lhd'+kst+',astr'
        errcode = Execute(command)
        
        mapdata[k].fn    = survey_label[k]
        mapdata[k].naxis = astr.naxis
        
        ;; Check if we have CROP boundaries.  If so, use these in
        ;;   mapdata, else compute from the headers.  If crop, extend
        ;;   borders by 0.01 degrees (36") in longitude space
        ;;   to correct for sloppy crop bounds.
        IF conf.hascrop THEN BEGIN
           
           mapdata[k].l = [lmin[k],lmax[k]] + [-0.01,0.01]
           mapdata[k].b = [bmin[k],bmax[k]]
           
           ;; Check for tiles spanning l=0
           IF mapdata[k].l[1] - mapdata[k].l[0] GE 300 THEN BEGIN
              mapdata[k].l = [mapdata[k].l[1],mapdata[k].l[0]] ; Reverse
              mapdata[k].l[0] -= 360.                          ; Negative lower
           ENDIF           
           
        ENDIF ELSE BEGIN
           mapdata[k].l = minmax((findgen(astr.naxis[0])-astr.crpix[0])*$
                                 astr.cd[0,0]+astr.crval[0])
           mapdata[k].b = minmax((findgen(astr.naxis[1])-astr.crpix[1])*$
                                 astr.cd[1,1]+astr.crval[1])
        ENDELSE
        ;; Save ASTR structure for each map to be used later
        command = 'astr'+kst+' = astr'
        errcode = Execute(command)
        
        ;; Keep memory clear
        undefine,lhd,astr
     ENDFOR
  ENDIF
  
  ;;  If we want to USEGRS...
  IF conf.usegrs THEN BEGIN
     ;;*************************************************************************
     ;; Check for and load in GRS information
     ;;
     ;; Quick checking of GRS information
     IF ~exist(v_std) THEN v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
     ;; Check if GRS file exists, but is not loaded
     IF (size(grs,/TYPE) EQ 8) EQ 0 THEN BEGIN
        
        ;; Load configuration structures to get necessary info
        IF ~n_elements(conf) THEN conf = omni_load_conf(cfile)
        IF ~n_elements(ancil) THEN $
           ancil = omni_read_conffile('./conffiles/ancillary.conf')
        
        ;; GRS spectrum block filename
        grssuf = nogrsfilt ? '_nofilt' : ''
        grsfn = 'local/'+conf.survey+'_grs_spectra_r'+$
                string(ancil.grs_rad,format="(I0)")+grssuf+'.sav'
        IF ~FILE_TEST(grsfn,/READ) THEN BEGIN
           message,'Warning: I need to run OMNI_GRS_SPECTRA_LOCAL to '+$
           'continue, this may take a while...',/inf
           wait,10
           omni_grs_spectra_local
        ENDIF
        message,'Restoring '+grsfn+' ...',/inf
        restore,grsfn,/ver
     ENDIF
     ;;*************************************************************************
  ENDIF
  
  ;;===========================================================
  ;; Create the velocity structure that will be saved to disk
  v = replicate( {cnum:0,$
                  l:0.d,$
                  b:0.d,$
                  mol:$
                  replicate({name:'',$
                             vlsr:0.d,$
                             lw:0.d,$
                             tmb:0.d,$
                             sigt:0.d,$
                             nspec:0,$
                             multiv:0b},nvel),$
                  grs: {vlsr:0.d,$
                        tpk:0.d,$
                        lw:0.d,$
                        tonspec:0.d,$
                        flag:0b},$
                  vlsr:-1000.d,$ ; This value will identify non-detections
                  lw:0.d,$
                  snr:0.d,$
                  multiv:0b,$
                  rv_types:0}, ncat)
  
  ;; Copy over structure elements
  v.cnum     = s.cnum
  v.l        = s.glon
  v.b        = s.glat
  v.mol.name = vel.name
  
  ;;===========================================================
  ;; Start looping through input velocity MRTs, and placing
  ;; information into the v.mol structures.
  FOR vi=0,nvel-1 DO BEGIN
     
     message,'Reading in velocity information for '+vel[vi].name+'...',/inf
     vs = omni_read_vel(vel[vi], nobs)
     
     ;; Loop over entries in the MRT
     FOR oi=0,nobs-1 DO BEGIN
        
        IF conf.haslabel THEN BEGIN
           ;; Use label map to identify survey source
           
           ;; Check this object's position against the mapdata
           ;;   structure, with cases for glon near 0 deg.
           hit = WHERE( vs[oi].l GE mapdata.l[0] AND $
                        vs[oi].l LE mapdata.l[1] AND $
                        vs[oi].b GE mapdata.b[0] AND $
                        vs[oi].b LE mapdata.b[1], nhit )
           vs[oi].l -= 360.     ; Check for, say, l = 359, map = -1
           hitm = WHERE( vs[oi].l GE mapdata.l[0] AND $
                         vs[oi].l LE mapdata.l[1] AND $
                         vs[oi].b GE mapdata.b[0] AND $
                         vs[oi].b LE mapdata.b[1], nhitm )
           vs[oi].l += 720.     ; Check for, say, l = -1, map = 359
           hitp = WHERE( vs[oi].l GE mapdata.l[0] AND $
                         vs[oi].l LE mapdata.l[1] AND $
                         vs[oi].b GE mapdata.b[0] AND $
                         vs[oi].b LE mapdata.b[1], nhitp )
           vs[oi].l -= 360.     ; Reset to original value
           
           ;; Check that we found something
           nmatch = nhit + nhitm + nhitp
           IF nmatch EQ 0 THEN BEGIN
              message,'Error: No map match for '+vel[vi].name+$
                      ' velocity pointing at (l,b) = '+$
                      string(vs[oi].l,vs[oi].b,$
                             format="('(',F0.4,',',F0.4,')')"),/inf
              CONTINUE
           ENDIF
           
           ;; Re-aggregate the results
           hits = hit
           IF nhitm THEN hits = [hits,hitm]
           IF nhitp THEN hits = [hits,hitp]
           ;; Remove any "-1" entries from hits 
           hits = hits[missing([-1],hits)]
           
           ;;===================================================================
           ;; If nmatch = 1, we are good to go, but we need to figure out
           ;;   which image should be used for objects with positions in
           ;;   more than one map.
           
           IF nmatch EQ 1 THEN k = hits ELSE BEGIN
              
              bestk = -1
              ;; Loop through the label maps, run adxy, check for non-zero
              ;;   value at location of peak flux density.
              FOR ll=0,nmatch-1 DO BEGIN
                 k = (hits[ll])[0]
                 kst = string(k,format="(I0)")
                 command = 'ad2xy,vs[oi].l,vs[oi].b,astr'+kst+',x,y'
                 errcode = Execute(command)
                 x = (long(round(x)))[0] ; New ad2xy spits arrays 
                 y = (long(round(y)))[0] ; even if scalar went in
                 IF( x LT 0 || x GE mapdata[k].naxis[0] ) THEN CONTINUE
                 IF( y LT 0 || y GE mapdata[k].naxis[1] ) THEN CONTINUE
                 command = 'val = label'+kst+'[x,y]'
                 errcode = Execute(command)
                 IF errcode NE 1 THEN STOP ; If something fucks up, stop running
                 IF val NE 0 THEN bestk = k
              ENDFOR  
              k = bestk
           ENDELSE              ; End of the nmatch > 1 checking section
           ;;===================================================================
           
           
           ;; Find aperture in the label map, and extract value(s)
           k = k[0]
           IF k EQ -1 THEN CONTINUE ; No survey object in more than 1 field
           kst = string(k,format="(I0)")
           
           ;; Construct the aperture based on the ASTROMETRY
           command = 'aperture = omni_aperture_mask(junk,ap_size,'+$
                     'vs[oi].l,vs[oi].b,ASTR=astr'+kst+')'
           errcode = Execute(command)
           IF ~errcode THEN print,[nmatch,hits] 
           label_ind = where(aperture GT 0, nli)
           IF nli EQ 0 THEN message,'You need a stiff drink, sir.'
           
           ;; Find the UNIQUE label values within the aperture
           command = 'values = label'+kst+'[label_ind]'
           errcode = Execute(command)
           values = values[UNIQ(values, SORT(values))]
           valid_ind = where(values NE 0, nvi)
           
           ;; If no survey object at this location, he can go about
           ;;   his business.  Move along, move along.
           IF nvi EQ 0 THEN CONTINUE
           
           
           ;; Loop through valid_ind to construct the array 'jj'
           jj = !null
           FOR ii=0,nvi-1 DO BEGIN
              
              labval = (values[valid_ind[ii]])[0] ; Scalar!
              
              ;; Go through SURVEY structure to find object in this field
              ;;   with the same LABVAL
              labstr = strmid(survey_label[k],$
                              strpos(survey_label[k],'/',/REVERSE_SEARCH)+1)
              surind = where(strmatch(survey.labelname, labstr), nsur)
              fieldind = where(survey[surind].labval EQ labval)
              
              ;; The 'j' we want is this object!
              jj = [jj,where(survey.cnum EQ survey[surind[fieldind]].cnum)]
           ENDFOR
           
        ENDIF ELSE BEGIN      ; End of USING LABEL MAPS -- If no LABEL, then...
           ;; Look for closest catalog object
           gcirc,2,vs[oi].l,vs[oi].b,s.glon,s.glat,dis
           
           ;; If too far away, skip
           IF min(dis,jj) GT conf.maxdist THEN CONTINUE
        ENDELSE
        
        ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;;BEAM-LABEL-BEAM-LABEL-BEAM-LABEL-BEAM-LABEL-BEAM-LABEL-BEAM-LABEL
        ;;
        ;; Loop through the catalog objects that lie within the
        ;; aperture, if applicable  
        FOR kk=0,n_elements(jj)-1 DO BEGIN
           
           ;;  j is the element in s that corresponds to the current entry
           j = (jj[kk])[0]      
           
           v[j].mol[vi].nspec++ ; Increment observations for this line, object
           
           IF v[j].mol[vi].nspec EQ 1 THEN BEGIN
              ;; If this is the first, just fill it in
              
              v[j].mol[vi].vlsr = vs[oi].vlsr
              v[j].mol[vi].lw   = vs[oi].lw
              v[j].mol[vi].tmb  = vs[oi].tmb
              v[j].mol[vi].sigt = vs[oi].sigt
              
           ENDIF ELSE BEGIN
              ;; If NSPEC is > 1, carefully add...
              
              ;; Check for multiv warning flag -- skip if necessary
              IF v[j].mol[vi].multiv THEN CONTINUE
              
              ;; Check if existing and new velocites are > 5 km/s apart
              IF abs(v[j].mol[vi].vlsr - vs[oi].vlsr) GT 5.d THEN BEGIN
                 ;; print,v[j].mol[vi].nspec, v[j].mol[vi].vlsr, vs[oi].vlsr
                 v[j].mol[vi].multiv = 1b ; Set warning flag
                 CONTINUE                 ; Skip to next velocity
              ENDIF
              
              ;; If velocites are okay, perform a weighted average
              ;;   according to tmb -- This means for combining more than
              ;;   two values is not exact, but differs from the true
              ;;   value by only ~1-2%.
              t1 = v[j].mol[vi].tmb * v[j].mol[vi].nspec ; Weight of previous
              t2 = vs[oi].tmb                            ; Weight of current
              
              v[j].mol[vi].vlsr=(t1*v[j].mol[vi].vlsr + t2*vs[oi].vlsr)/(t1+t2)
              v[j].mol[vi].lw  =(t1*v[j].mol[vi].lw   + t2*vs[oi].lw)  /(t1+t2)
              v[j].mol[vi].tmb =(t1*v[j].mol[vi].tmb  + t2*vs[oi].tmb) /(t1+t2)
              v[j].mol[vi].sigt=(t1*v[j].mol[vi].sigt + t2*vs[oi].sigt)/(t1+t2)
              
           ENDELSE
           
        ENDFOR                  ; End of loop through objects within aperture
        
     ENDFOR                     ; End of loop through sources in MRT
     
  ENDFOR                        ; End of loop over velocity MRTs
  
  
  ;;=======================================================================
  ;; Combine information from the various spectroscopic catalogs into
  ;;   a single value for each catalog source
  message,'Merging spectroscopic information for each object...',/inf
  
  v_std = findgen(conf.nvbin)*conf.deltav + conf.vstart
  
  ;; Loop over catalog sources
  FOR ci=0,ncat-1 DO BEGIN
     
     ;;GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS
     IF conf.usegrs THEN BEGIN
        ;;***************************************************************
        ;; Add info from the GRS 13CO sepectra into the structure
        ic = where(grs.cnum EQ v[ci].cnum, nc)
        
        IF nc THEN BEGIN        ; Skip if can't find in GRS
           spectrum = grs[ic].spectrum
           CASE grs[ic].flag OF
              0:                ; Do nothing, this will be caught below
              1: BEGIN          ; Okay ON-OFF from OMNI_GRSMATCH -- mask
                 omni_grs_maskspec, spectrum, grsflag
                 v[ci].grs.flag = grsflag
              END
              4: BEGIN          ; Landlocked ONSPEC only
                 spectrum = grs[ic].onspec
                 omni_grs_maskspec, spectrum, grsflag
                 CASE grsflag OF
                    0: v[ci].grs.flag = 7 ; Nothing worth anything
                    1: v[ci].grs.flag = 4  ; If okay, set flag = 4
                    2: v[ci].grs.flag = 5  ; If multiple, set flag = 5
                    3: v[ci].grs.flag = 6  ; If indistinguishable, set flag = 6
                    ELSE: message,"It's the end of the world as we know it, "+$
                                  "and I feel fine."
                 ENDCASE
              END
              ELSE: message,'See a head shrinker and a bartender for therapy.'
           ENDCASE
           
           IF total(spectrum) NE 0. THEN BEGIN 
              yf = mpfitpeak(v_std,spectrum,A,NTERM=3)
              v[ci].grs.vlsr    = A[1]
              v[ci].grs.lw      = A[2]
              v[ci].grs.tpk     = max(spectrum,grsj)
              v[ci].grs.tonspec = grs[ic].onspec[grsj]
           ENDIF
        ENDIF
        ;;*************************************************************** 
     ENDIF
     ;;GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS-GRS
     
     ;; Skip to next object if no dense gas spectral information
     IF total(v[ci].mol.tmb) EQ 0 THEN CONTINUE
     
     ;; Count number of spectra added
     nspec = 0
     
     FOR vi=0,nvel-1 DO BEGIN   ; Loop over molecular lines
        
        ;; Check to see if this object was detected in this line AND
        ;; Check if MULTIV flag set to skip to next molecular line
        IF (v[ci].mol[vi].tmb EQ 0) || (v[ci].mol[vi].multiv) THEN CONTINUE
        
        nspec++                  ; Increment counter
        v[ci].rv_types += 2^(vi) ; Add RV type flag
        
        ;; Check for finite linewidth... if not then set to something
        ;;   reasonable (say, 3 km/s).
        IF ~finite(v[ci].mol[vi].lw) THEN v[ci].mol[vi].lw = 3.d
        ;; Check for reasonable linewidth... set upper bound = 10 km/s
        ;;   -- value from histogram of LW
        v[ci].mol[vi].lw = v[ci].mol[vi].lw < 10.
        
        IF nspec EQ 1 THEN BEGIN
           ;; If this is the first molecular line, just fill it in
           
           v[ci].vlsr = v[ci].mol[vi].vlsr
           v[ci].lw   = v[ci].mol[vi].lw
           v[ci].snr  = v[ci].mol[vi].tmb / v[ci].mol[vi].sigt
           
        ENDIF ELSE BEGIN
           ;; Otherwise, carefully add...
           
           ;; Check for multiv warning flag -- skip if necessary
           IF v[ci].multiv THEN CONTINUE
           
           ;; Check for velocity match (<= 5 km/s)
           IF abs(v[ci].vlsr - v[ci].mol[vi].vlsr) GT 5.d THEN BEGIN
              v[ci].multiv = 1b ; Set warning flag
              CONTINUE          ; Skip to next molecular line
           ENDIF
           
           ;; If velocities are okay, perform a weighted average
           ;;   according to snr -- This means for combining more than
           ;;   two values is not exact, but differs from the true
           ;;   value by only ~1-2%.
           t1 = v[ci].snr * nspec                      ; Weight of previous
           t2 = v[ci].mol[vi].tmb / v[ci].mol[vi].sigt ; Weight of current
           
           v[ci].vlsr = (t1*v[ci].vlsr + t2*v[ci].mol[vi].vlsr) / (t1+t2)
           v[ci].lw   = (t1*v[ci].lw   + t2*v[ci].mol[vi].lw)   / (t1+t2)
           v[ci].snr  = (t1*v[ci].snr  + t2*t2)                 / (t1+t2)
           
        ENDELSE
        
     ENDFOR                     ; End of loop over Molecular Lines
     
     ;; Check if MULTIV flag is set --> set vlsr to non-detection &
     ;;    set flag
     IF v[ci].multiv THEN BEGIN
        v[ci].vlsr = -1000.d
        v[ci].rv_types = -1
     ENDIF
     
  ENDFOR                        ; End of loop over CATALOG sources
  
  
  ;; Save all this to disk for later joy
  save,v,filename='local/'+conf.survey+'_velocities.sav',/verbose
  
  ;; Clean up the memory
  undefine,v
  
  RETURN
END
