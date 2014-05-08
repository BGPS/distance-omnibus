;+
; NAME:
;       OMNI_EXPORT_FITS
;
; PURPOSE:
;       Takes the IDL structure contained in (conf.survey)_pvec.sav
;       and outputs a FITS file following the specifications in
;       dpdf_params.conf.
;
; CATEGORY:
;       distance-omnibus Utility
;
; CALLING SEQUENCE:
;       OMNI_EXPORT_FITS [,fn][,CONFFILE=cfile]
;
; INPUTS:
;       NONE
;
; OPTIONAL INPUTS:
;       FN --       Name of the IDL save file containing the pvec structure
;                   to be exported [Default: ./local/{survey}_pvec.sav] 
;       CONFFILE -- Name of the configuration file to use for survey
;                   information [Default: conffiles/survey_info.conf]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE  (FITS file ./local/{survey}_dpdf_table.fits written to
;              disk.)
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       OMNI_CONFIG -- The set of configuration structures, read in
;                      from the config files in conffiles/
;
; MODIFICATION HISTORY:
;
;       Created:  03/20/13, TPEB -- Initial version.
;       Modified: 03/21/13, TPEB -- Added earlier check for dpdfs.fits
;                                   = 1b so that nothing is read in if
;                                   we don't even want FITS output.
;       Modified: 09/03/13, TPEB -- Make it work mo' better.
;       Modified: 09/18/13, TPEB -- 
;       Modified: 10/16/13, TPEB -- Added /ALL keyword to force
;                                   writing out of all data.
;       Modified: 10/23/13, TPEB -- Skip inclusion of the velocity
;                                   data if SURVEY has no kinematic
;                                   information.
;       Modified: 10/24/13, TPEB -- Conformal changes to allow non-use
;                                   of GRS 13CO, and added contents
;                                   information to the output FITS
;                                   file.
;       Modified: 12/13/13, TPEB -- Added DPDF_PARAMS conffile element
;                                   to specify /ALL from the
;                                   configuration file rather than
;                                   just command-line input.
;       Modified: 05/08/14, TPEB -- Removed the 'help' command at the
;                                   end, as it produces output that
;                                   may confound other users.
;
;-

PRO OMNI_EXPORT_FITS, fn, CONFFILE=cfile, ALL=all
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  
  ;; Read in configuration files & PVEC structure -- check to see if
  ;;   we even wanted to do this in the first place!
  conf = omni_load_conf(cfile)
  IF ~exist(dpdfs) THEN $
     dpdfs = omni_read_conffile('./conffiles/dpdf_params.conf')
  IF ~dpdfs.fits THEN RETURN
  IF n_elements(fn) EQ 0 THEN fn = './local/'+conf.survey+'_pvec.sav'
  restore,fn,/ver
  IF conf.hasvelocity THEN $
     restore,'./local/'+conf.survey+'_velocities.sav',/ver
  
  ;; Parse keyword input
  all = dpdfs.save_all || KEYWORD_SET(all)
  
  ;; Write out constrained data to a FITS file
  fitsout = bytarr(n_tags(dpdfs.fitsout))
  FOR ll=0,n_tags(dpdfs.fitsout)-1 DO fitsout[ll] = dpdfs.fitsout.(ll)
  iout = where(fitsout,nout)
  tags = tag_names(constrain)
  IF nout THEN BEGIN
     ftags = (tag_names(dpdfs.fitsout))[iout]
     
     IF ~all THEN BEGIN
        ;; Winnow the pvec structure to those requested for output
        FOR ii=0,nout-1 DO BEGIN
           
           IF ftags[ii] EQ 'VLSR' THEN BEGIN 
              
              ;; Check that kinematic info (or KNOWND) has any sources attached
              constrain_arg = constrain.kdist
              IF conf.usegrs THEN $
                 constrain_arg = constrain_arg OR constrain.grsmatch
              IF dpdfs.dpdf.knownd NE 0 THEN $
                 constrain_arg = constrain_arg OR constrain.knownd
              keepi = where(constrain_arg, nkp)
              
           ENDIF ELSE BEGIN
              ;; Check that this tag exists in CONSTRAIN
              cind = where(tags EQ ftags[ii],nc)
              IF ~nc THEN BEGIN
                 message,'Error: Requested DPDF constraint '+ftags[ii]+$
                         ' for FITS output (dpdf-params: has_*) has no '+$
                         'matches amongst source objects.  Exiting.',/cont
                 RETURN
              ENDIF
              ;; Check that this tag has any sources attached
              keepi = where(constrain.(cind),nkp)
           ENDELSE
           
           ;; Error check
           IF ~nkp THEN BEGIN
              message,'Error: Requested DPDF constraint '+ftags[ii]+' for '+$
                      'FITS output (dpdf-params: has_*) has no matches '+$
                      'amongst source objects.  Exiting.',/cont
              RETURN
           ENDIF
           pvec      = pvec[keepi]
           constrain = constrain[keepi]
           IF conf.hasvelocity THEN v = v[keepi]
           message,'For FITS output, using '+ftags[ii]+' constraint, kept '+$
                   strtrim(nkp,2)+' objects.',/inf
        ENDFOR      
     ENDIF
     
     ;;  Get structure info from pvec and begin writting
     ntag = n_tags(pvec)
     fn = './local/'+conf.survey+'_dpdf_table.fits'
     
     mwrfits,constrain,fn,hd,/create,/no_comment,STATUS=status
     hdr_leg = 'HDU #2: Table of constraints for each object' ; Header legend
     print,tag_names(pvec)
     mwrfits,{nbins:pvec[0].nbins,binsize:pvec[0].binsize,$
              binstart:pvec[0].binstart},fn,/no_comment
     hdr_leg = [hdr_leg,'HDU #3: Common distance scale parameters for DPDFs']
     ptags = tag_names(pvec)
     FOR kk=0,ntag-8 DO BEGIN
        mwrfits,pvec.(kk+1),fn,/no_comment,/no_types
        hdr_leg = [hdr_leg,'HDU #'+string(kk+4,format="(I0)")+': '+$
                   ptags[kk+1]+' DPDFs']
     ENDFOR
     lasthdu = kk
     mwrfits,pvec.stat,fn,/no_comment
     hdr_leg = [hdr_leg,'HDU #'+string(lasthdu+4,format="(I0)")+$
                ': Distance statistics (all distances in pc)']
     
     ;;*******************************************************************
     ;; De-Nest the velocity structure for writing, if we have
     ;;   kinematic information for this survey.
     IF conf.hasvelocity THEN BEGIN
        vst = {cnum:0L,$
               l: 0.d,$
               b:0.d,$
               vlsr:0.d,$
               lw:0.d,$
               snr:0.d,$
               multiv:0b,$
               rv_types:0L}
        FOR kk=0,n_elements(v[0].mol)-1 DO BEGIN ; Dense Gas Spectra
           kst = string(kk,format="(I0)")
           vst = create_struct(vst,$
                               'MOL'+kst+'_NAME',v[0].mol[kk].name,$
                               'MOL'+kst+'_VLSR',v[0].mol[kk].vlsr,$
                               'MOL'+kst+'_LW',v[0].mol[kk].lw,$
                               'MOL'+kst+'_TMB',v[0].mol[kk].tmb,$
                               'MOL'+kst+'_SIGT',v[0].mol[kk].sigt,$
                               'MOL'+kst+'_NSPEC',v[0].mol[kk].nspec,$
                               'MOL'+kst+'_MULTIV',v[0].mol[kk].multiv)
        ENDFOR
        vst = create_struct(vst,$ ; GRS Spectra
                            'GRS_VLSR',v[0].grs.vlsr,$
                            'GRS_TPK',v[0].grs.tpk,$
                            'GRS_LW',v[0].grs.lw,$
                            'GRS_TONSPEC',v[0].grs.tonspec,$
                            'GRS_FLAG',v[0].grs.flag)                         
        vst = replicate(vst, n_elements(v))
        
        ;; Load in the values
        vst.cnum     = v.cnum
        vst.l        = v.l
        vst.b        = v.b
        vst.vlsr     = v.vlsr
        vst.lw       = v.lw
        vst.snr      = v.snr
        vst.multiv   = v.multiv
        vst.rv_types = v.rv_types
        FOR kk=0,n_elements(v[0].mol)-1 DO BEGIN ; Dense Gas Spectra
           kst = string(kk,format="(I0)")
           command = 'vst.mol'+kst+'_name = v.mol[kk].name'
           err = Execute(command)
           command = 'vst.mol'+kst+'_vlsr = v.mol[kk].vlsr'
           err = Execute(command)
           command = 'vst.mol'+kst+'_lw = v.mol[kk].lw'
           err = Execute(command)
           command = 'vst.mol'+kst+'_tmb = v.mol[kk].tmb'
           err = Execute(command)
           command = 'vst.mol'+kst+'_sigt = v.mol[kk].sigt'
           err = Execute(command)
           command = 'vst.mol'+kst+'_nspec = v.mol[kk].nspec'
           err = Execute(command)
           command = 'vst.mol'+kst+'_multiv = v.mol[kk].multiv'
           err = Execute(command)
        ENDFOR
        ;; GRS Spectra
        command = 'vst.grs_vlsr = v.grs.vlsr'
        err = Execute(command)
        command = 'vst.grs_tpk = v.grs.tpk'
        err = Execute(command)
        command = 'vst.grs_lw = v.grs.lw'
        err = Execute(command)
        command = 'vst.grs_tonspec = v.grs.tonspec'
        err = Execute(command)
        command = 'vst.grs_flag = v.grs.flag'
        err = Execute(command)
        ;;*******************************************************************
        
        mwrfits,vst,fn,/no_comment
        hdr_leg = [hdr_leg,'HDU #'+string(lasthdu+5,format="(I0)")+$
                ': Velocity catalog']
     ENDIF
     
     
     ;;========================================================
     ;; Add COMMENTs to HDU #1 with information about the file
     head = headfits(fn)
     
     hdr_his = ['==============================',$
                'This FITS file was created by OMNI_EXPORT_FITS',$
                'on '+systime(0)]
     hdr_leg = ['==============================',$
                'Contains DPDFs for '+string(n_elements(pvec),format="(I0)")+$
                ' sources from '+conf.survey,$
                'The DPDF tables are arranged as NBIN x Nsource images, and',$
                'the constraint and distance tables are Nsource long.',$
                '==============================',$
                ' Description of the HDUs for this FITS file:',$
                hdr_leg]
     
     
     sxaddhist,hdr_his,head
     sxaddhist,hdr_leg,head,/comment
     modfits,fn,0,head
     
  ENDIF ELSE message,'For mass-function computation, no additional '+$
                     'constraints beyond a kinematic distance are requested.',$
                     /inf
  
  undefine,pvec,constrain
  
  RETURN
END
