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
     'PARALLAX': RETURN,'PUR5'
     'HRDS':     RETURN,'ORG7'
     'POST':     RETURN,'Black'
     ELSE:       RETURN,'Deep Pink'
  ENDCASE
END


PRO OMNI_PLOT_DPDFS, p, BASECS=basecs
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE
  
  COMMON OMNI_CONFIG, conf, mw, local, dpdfs, ancil, fmt, conffile
  COMMON DPLOT_BLOCK, fmt2
  
  ;; Parse input
  IF ~n_elements(basecs) THEN basecs = 1.0
  
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
  
  ;; Set up the plotting environment
  cgPlot,/nodata,charsize=1.0*basecs,d,p.(1),yr=[0,ymax],$
         xtit='Heliocentric Distance  [kpc]',$
         ytit='Probability per '+string(p.binsize,format="(I0)")+'-pc bin',$
         tit=string(p.cnum,p.glon,p.glat,format=$
                          "('"+conf.survey+" #',"+fmt2+$
                          ",'  l = ',F5.1,'  b = ',F5.2)")
  
  ;; Plot the Tangent Distance -- for INNER GALAXY objects
  do_dtan = p.stat.dtan GT 0
  IF do_dtan THEN vline,p.stat.dtan/1.d3,color='brown'
  
  ;; Plot the requisite DPDFs, leaving out the uniform priors
  tags   = !null
  legcol = !null
  FOR ii=1,ndpdf DO BEGIN
     IF min(p.(ii)) EQ max(p.(ii)) THEN CONTINUE
     tags = [tags,tnames[ii]]
     legcol = [legcol,omni_plot_dpdfs_color(tnames[ii])]
     cgOplot,d,p.(ii),color=omni_plot_dpdfs_color(tnames[ii])
  ENDFOR
  
  ;; Add various notations to the plot
  IF do_dtan THEN cgText,p.stat.dtan/1.d3+0.1,0.90*!y.crange[1],'d!dtan!n',$
                         charsize=0.8*basecs,color='brown'
  
  al_legend,/top,/right,linsize=1.0,tags,color=legcol,charsize=0.8*basecs,$
            linestyle=0,box=0
    
  cgText,15,0.5*!y.crange[1],'P!dML!n = '+string(p.stat.pml,format="(F0.3)"),$
         charsize=0.9*basecs
  
  RETURN
END
