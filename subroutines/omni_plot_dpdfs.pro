;+
; NAME:
;       OMNI_PLOT_DPDFS
;
; PURPOSE:
;       With the input of a single-element PVEC structure, produce a
;       plot of the DPDFs for diagnostic purposes.  This routine
;       assumes the plotting environment is already extant -- i.e. it
;       does not open a new window or open a PostScript file for
;       writing.
;
; CATEGORY:
;       distance-omnibus Diagnostic utility
;
; CALLING SEQUENCE:
;       OMNI_PLOT_DPDFS, pvec
;
; INPUTS:
;       PVEC -- The single-element structure containing the DPDFs for
;               a single catalog object.
;
; OPTIONAL INPUTS:
;       BASECS -- Base charsize to be used for plotting [Default: 1.0]
;       BASELS -- Base linsize to be used for al_legend [Default: 1.0]
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       NONE
;
; OPTIONAL OUTPUTS:
;       NONE
;
; COMMON BLOCKS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  12/08/13, TPEB -- Initial version.
;       Modified: 12/11/13, TPEB -- Renamed MASER probability to
;                                   PARALLAX for clarity.
;       Modified: 12/12/13, TPEB -- Added color for HRDS proir DPDF.
;       Modified: 04/11/14, TPEB -- Add additional distance diagnostic
;                                   output.
;       Modified: 09/05/14, TPEB -- Plotting changes.
;       Modified: 09/29/14, TPEB -- Mucking around for the paper.
;       Modified: 11/02/14, TPEB -- More mucking.
;
;-

FUNCTION OMNI_PLOT_DPDFS_COLOR, tagname
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Use a CASE statement to choose the color name for DPDFs
  CASE strupcase(tagname) OF
     'KDIST':    RETURN,'Green'
     'GRSMATCH': RETURN,'Goldenrod'
     'H2':       RETURN,'BLU5'
     'EMAF':     RETURN,'Crimson'
     'KNOWND':   RETURN,'BLK5'
     'PARALLAX': RETURN,'BLK5'
     'HRDS':     RETURN,'PUR7'
     'POST':     RETURN,'Black'
     ELSE:       RETURN,'Deep Pink'
  ENDCASE
END


PRO OMNI_PLOT_DPDFS, p, BASECS=basecs, BASEls=basels, PAPER=paper, $
                     UL=ul, UR=ur, LL=ll, LR=lr
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON DPLOT_BLOCK, fmt2
  
  ;; Parse input
  IF ~n_elements(basecs) THEN basecs = 1.0
  IF ~n_elements(basels) THEN basels = 1.0
  
  ;; Parse panel information, if PAPER
  IF KEYWORD_SET(paper) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(ul): BEGIN
           xtit = ''
           ytit = ''
           xr=[0,19.999]
        END
        KEYWORD_SET(ur): BEGIN
           xtit = ''
           ytit = ''
           xr=[0,20]
        END
        KEYWORD_SET(ll): BEGIN
           xtit = ''
           ytit = ''
           xr=[0,19.999]
        END
        KEYWORD_SET(lr): BEGIN
           xtit = ''
           ytit = ''
           xr=[0,20]
        END
        ELSE: message,'You MUST specify a panel if you want /PAPER.'
     ENDCASE
  ENDIF ELSE BEGIN
     xtit='Heliocentric Distance  [kpc]'
     ytit='Probability per '+string(p.binsize,format="(I0)")+'-pc bin'
     xr = [0,20]
  ENDELSE
  
  ;; Load configuration files, as necessary
  IF ~exist(conf) THEN conf = omni_load_conf()
  IF ~exist(fmt2) THEN BEGIN
     s = omni_read_cat(conf.cat,ncat)
     fmt2 = string(ceil(alog10(ncat+1)),format="('I',I0)")
  ENDIF
  
  ;; Find the number of DPDFs contained in the PVEC structure
  ndpdf  = n_tags(p) - 7
  tnames = tag_names(p)
  
  message,'Structure has '+string(ndpdf,format="(I0)")+' DPDFs',/inf
  
  ;; Determine the YRANGE for the plot from the maximum value of any
  ;;   of the DPDFs
  ymax = 0.
  FOR ii=1,ndpdf DO ymax = ymax > max(p.(ii))
  
  ;; Set up the distance array...
  d = ( dindgen(p.nbins) * p.binsize + p.binstart ) / 1.d3 ; [kpc]

  
  tit = ~KEYWORD_SET(paper) ? string(p.cnum,p.glon,p.glat,format=$
                                     "('"+conf.survey+" #',"+fmt2+$
                                     ",'  l = ',F5.1,'  b = ',F5.2)") : ''
  
  yr = KEYWORD_SET(paper) ? [0,1.1] : [0,ymax]
  ;; Set up the plotting environment
  cgPlot,/nodata,charsize=1.0*basecs,d,p.(1),yr=yr,xtit=xtit,ytit=ytit,tit=tit,$
         xr=xr,/xst
  
  ;; Plot the Tangent Distance -- for INNER GALAXY objects
  do_dtan = p.stat.dtan GT 0
  IF do_dtan THEN vline,p.stat.dtan/1.d3,color='brown'
  
  ;; Plot the requisite DPDFs, leaving out the uniform priors
  tags   = !null
  legcol = !null
  legls  = !null
  legthk = !null
  norm = KEYWORD_SET(paper) ? ymax : 1.0
  FOR ii=1,ndpdf DO BEGIN
     IF min(p.(ii)) EQ max(p.(ii)) THEN CONTINUE
     tags = [tags,translate_dpdf_tag(tnames[ii],/IDL)]
     thick = 2.0 + 1.0*(ii EQ ndpdf)
     CASE tnames[ii] OF
        'H2': BEGIN
           linestyle = 1
           p.(ii) *= 4.
        END
        'EMAF': BEGIN
           linestyle = 2
           p.(ii) *= 3.
        END
        'PARALLAX': BEGIN
           linestyle = 5
           p.(ii) *= 1.0
        END
        'HRDS': BEGIN
           linestyle = 3
           p.(ii) *= 1.
        END
        ELSE   : linestyle = 0
     ENDCASE
     legcol = [legcol,omni_plot_dpdfs_color(tnames[ii])]
     legls =  [legls,linestyle]
     legthk = [legthk,thick]
     cgOplot,d,p.(ii)/norm,color=omni_plot_dpdfs_color(tnames[ii]),$
             thick=thick,linestyle=linestyle
  ENDFOR
  
  ;; Add various notations to the plot
  IF do_dtan THEN cgText,p.stat.dtan/1.d3+0.1,0.90*!y.crange[1],'d!dtan!n',$
                         charsize=0.8*basecs,color='brown'
  
  name = 'G'+string(p.glon,p.glat,format="(F07.3,F+07.3)")
  
  al_legend,/top,/right,linsize=basels,['',tags],color=['background',legcol],$
            charsize=0.8*basecs,thick=[1,legthk],$
            linestyle=[0,legls],box=0,spacing=basecs*1.1
  al_legend,/top,/right,[name],$
            charsize=0.9*basecs,$
            box=0,spacing=basecs*0.8



  IF KEYWORD_SET(paper) THEN BEGIN
     cgText,14,0.35*!y.crange[1],charsize=0.85*basecs,'P!dML!n = '+$
            string(p.stat.pml,format="(F0.2)")
     IF p.stat.fw68/1.d3 GE 2.3 THEN $
        cgText,14,0.25*!y.crange[1],charsize=0.85*basecs,'d'+cgSymbol('sun')+$
               ' = ---- kpc' $
     ELSE $
        cgText,14,0.25*!y.crange[1],charsize=0.85*basecs,'d'+cgSymbol('sun')+$
               ' = '+string(p.stat.duse[0]/1.d3,format="(F0.2)")+' kpc'
     cgText,14,0.15*!y.crange[1],charsize=0.85*basecs,'FW!d68!n = '+$
            string(p.stat.fw68/1.d3,format="(F0.2)")+' kpc'
  ENDIF ELSE BEGIN
     cgText,15,0.50*!y.crange[1],charsize=0.85*basecs,'P!dML!n = '+$
            string(p.stat.pml,format="(F0.3)")
     cgText,15,0.45*!y.crange[1],charsize=0.85*basecs,'d!dML!n = '+$
            string(p.stat.dml[0]/1.d3,format="(F0.3)")+' kpc'
     cgText,15,0.40*!y.crange[1],charsize=0.85*basecs,'d!dbar!n = '+$
            string(p.stat.dbar[0]/1.d3,format="(F0.3)")+' kpc'
     cgText,15,0.35*!y.crange[1],charsize=0.85*basecs,'d!duse!n = '+$
            string(p.stat.duse[0]/1.d3,format="(F0.3)")+' kpc'
     cgText,15,0.30*!y.crange[1],charsize=0.85*basecs,'FW!d68!n = '+$
            string(p.stat.fw68/1.d3,format="(F0.3)")+' kpc'
  ENDELSE
  
  RETURN
END
