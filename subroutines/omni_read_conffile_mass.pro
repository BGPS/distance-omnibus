;+
; NAME:
;       OMNI_READ_CONFFILE_MASS
;
; PURPOSE:
;       Parses the MASS-type configuration file.
;
; CATEGORY:
;       distance-omnibus Configuration File Subroutine
;
; CALLING SEQUENCE:
;       conf = OMNI_READ_CONFFILE_MASS( lun, conffile, lnum )
;
; INPUTS:
;       LUN      -- The logical unit number for the configuration file.
;       CONFFILE -- Name of the configuration file, used only for
;                   error display.
;       LNUM     -- Line number, used only for error display.
;
; OPTIONAL INPUTS:
;       NONE
;
; KEYWORD PARAMETERS:
;       NONE
;
; OUTPUTS:
;       CONF -- Structure containing the fields in the configuration
;               file.
;
; OPTIONAL OUTPUTS:
;       NONE
;
; MODIFICATION HISTORY:
;
;       Created:  02/19/13, TPEB -- Initial version.
;       Modified: 02/21/13, TPEB -- Split out parsing of individual
;                                   config types into subfunctions.
;       Modified: 04/03/13, TPEB -- Added mass-function parameters in
;                                   MASS type configuration file.
;       Modified: 04/17/13, TPEB -- For MASS type, changed temperature
;                                   distribution to lognormal from
;                                   Gaussian.
;       Modified: 05/03/13, TPEB -- Added mmax to MASS type.
;       Modified: 05/07/13, TPEB -- Added dmfbinw to MASS type.
;       Modified: 08/21/13, TPEB -- Updating MASS type for use with
;                                   the GRSMATCH kinematic distance
;                                   likelihood DPDF.
;       Modified: 02/06/14, TPEB -- Split out this auxillary routine
;                                   from OMNI_RESD_CONFFILE.pro into a
;                                   separate file.
;       Modified: 02/06/14, TPEB -- Added Physical Quantity PDF
;                                   property keywords.
;
;-

;;=========================================================================
;; Function for the parsing of mass-deriv configuration files.
FUNCTION OMNI_READ_CONFFILE_MASS, lun, conffile, lnum
  
  COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN
  
  ;; Create the configuration structure for TYPE galactic
  conf = {need: {h2:0b,$
                 emaf:0b,$
                 nirex:0b,$
                 hisa:0b,$
                 irdc:0b,$
                 parallax:0b,$
                 knownd:0b,$
                 mst:0b},$
          dmin:0.,$
          dmax:0.,$
          rmin:0.,$
          rmax:0.,$
          nmin:0.,$
          nmax:0.,$
          sampletd:0b,$
          tdmu:0.,$
          tdfwhm:0.,$
          mmax:0.,$
          dmfbinw:0.d,$
          nmbin:0L,$
          nrbin:0L,$
          nnbin:0L,$
          mbinsize:0.d,$
          rbinsize:0.d,$
          nbinsize:0.d,$
          mbinstart:0.d,$
          rbinstart:0.d,$
          nbinstart:0.d,$
          error:''}
  
  ;; Continue reading the configuration file
  WHILE ~EOF(lun) DO BEGIN
     
     key = OMNI_READ_CONFFILE_READLINE(lun, conffile, lnum, val, nword, skip)
     IF skip THEN CONTINUE
     
     CASE key OF
        
        ;; Required DPDFs for mass computation
        'h2':       conf.need.h2       = (nword EQ 1) ? 0b : long(val) < 1
        'emaf':     conf.need.emaf     = (nword EQ 1) ? 0b : long(val) < 1
        'nirex':    conf.need.nirex    = (nword EQ 1) ? 0b : long(val) < 1
        'hisa':     conf.need.hisa     = (nword EQ 1) ? 0b : long(val) < 1
        'irdc':     conf.need.irdc     = (nword EQ 1) ? 0b : long(val) < 1
        'parallax': conf.need.parallax = (nword EQ 1) ? 0b : long(val) < 1
        'mst':      conf.need.mst      = (nword EQ 1) ? 0b : long(val) < 1
        'knownd':   conf.need.knownd   = (nword EQ 1) ? 0b : long(val) < 1
        
        ;; Physical constraints for inclusion in mass computation
        'dmin':   conf.dmin     = (nword EQ 1) ? 0. : float(val)
        'dmax':   conf.dmax     = (nword EQ 1) ? 0. : float(val)
        'rmin':   conf.rmin     = (nword EQ 1) ? 0. : float(val)
        'rmax':   conf.rmax     = (nword EQ 1) ? 0. : float(val)
        'nmin':   conf.nmin     = (nword EQ 1) ? 0. : float(val)
        'nmax':   conf.nmax     = (nword EQ 1) ? 0. : float(val)
        
        ;; Mass-fuction construction parameters
        'sampletd': conf.sampletd = (nword EQ 1) ? 0b : long(val) < 1
        'tdmu':     conf.tdmu     = float(val)
        'tdfwhm':   conf.tdfwhm   = float(val)
        'mmax':     conf.mmax     = (nword EQ 1) ? 0.  : float(val)
        'dmfbinw':  conf.dmfbinw  = (nword EQ 1) ? 0.d : double(val)
        
        ;; Physical property PDF construction parameters
        'nmbin':     conf.nmbin     = (nword EQ 1) ? 0L  : long(val)
        'nrbin':     conf.nrbin     = (nword EQ 1) ? 0L  : long(val)
        'nnbin':     conf.nnbin     = (nword EQ 1) ? 0L  : long(val)
        'mbinsize':  conf.mbinsize  = (nword EQ 1) ? 0.d : double(val)
        'rbinsize':  conf.rbinsize  = (nword EQ 1) ? 0.d : double(val)
        'nbinsize':  conf.nbinsize  = (nword EQ 1) ? 0.d : double(val)
        'mbinstart': conf.mbinstart = (nword EQ 1) ? 0.d : double(val)
        'rbinstart': conf.rbinstart = (nword EQ 1) ? 0.d : double(val)
        'nbinstart': conf.nbinstart = (nword EQ 1) ? 0.d : double(val)

        
        ELSE:BEGIN        
           message,'Unknown keyword >'+key+'< on line '+$
                   strtrim(lnum,2)+' in conffile '+conffile,/cont
           RETURN,{survey:'ERROR:',cat:'',error:'Unknown keyword'}
        END
     ENDCASE
  ENDWHILE
  
  RETURN,conf
END
