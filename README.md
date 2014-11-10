distance-omnibus
================

### Description

This is the source repository for the Bolocam Galactic Plane Survey (BGPS) effort to resolve distance measurements to catalog sources. Through the Bayesian application of prior Distance Probability Density Functions (DPDFs) derived from ancillary data to a kinematic distance likelihood, this code derives posterior DPDFs for catalog sources. This methodology is generalized for use with any (sub-)millimeter survey of the Galactic plane. 

The methodology was introduced in [Ellsworth-Bowers et al. (2013, ApJ, 770, 39)](http://adsabs.harvard.edu/abs/2013ApJ...770...39E) and demonstrated on the BGPS version 1 data [(Aguirre et al. 2011, ApJS, 192, 4)](http://adsabs.harvard.edu/abs/2011ApJS..192....4A).  An expansion of the distance methodology to include a new kinematic distance likelihood and prior DPDFs is presented in [Ellsworth-Bowers et al. (2014, ApJ, in press)](http://casa.colorado.edu/~ellswotp/BGPS_12/bgps12.pdf), and demonstrated on the re-reduced BGPS version 2 data of [Ginsburg et al. (2013, ApJS, 208, 14)](http://adsabs.harvard.edu/abs/2013ApJS..208...14G).

=======
### Software Requirements

This package is written entirely in the [Interactive Data Language (IDL)](http://www.exelisvis.com/ProductsServices/IDL.aspx), and requires a recent version (8.0 or higher) to run.

Several external libraries of IDL routines are also required to run **distance-omnibus**.  These libraries must be installed on the local machine and their paths included in the IDL path.  The **distance-omnibus** code assumes you have a version of these libraries *no older* than the version current as of the release date shown below.
   * IDLASTRO (http://idlastro.gsfc.nasa.gov/) or (https://github.com/wlandsman/IDLAstro)
   * The Coyote Graphics System (http://www.idlcoyote.com/idldoc/cg/index.html) or (https://code.google.com/p/idl-coyote/)
   * The Markwardt IDL Library (http://www.physics.wisc.edu/~craigm/idl/)


=======
### Data Requirements

##### BGPS-Produced Data 

For the Eight-Micron Absorption Feature (EMAF) DPDF method, the distribution of Galactic mid-infrared emission described in [Ellsworth-Bowers et al. (2013)](http://adsabs.harvard.edu/abs/2013ApJ...770...39E), computed from the model of [Robitaille et al. (2012, A&A, 545, 39](http://adsabs.harvard.edu/abs/2012A%26A...545A..39R), is required.  The **distance-omnibus** code uses a FITS file of this distribution, computed using the [Janus supercomputer](https://www.rc.colorado.edu/services/compute/janus).  The FITS cube of *foreground emission fraction* may be found at [http://casa.colorado.edu/~ellswotp/BGPS_12/MW_model_ffore.fits](http://casa.colorado.edu/~ellswotp/BGPS_12/MW_model_ffore.fits).

Additionally, for the EMAF method, star-subtracted versions of the *Spitzer*/GLIMPSE IRAC Band 4 images (see below) are required.  It is infeasible to host a copy of these files, but the code to produce them is included in this distribution.  Before running the **distance-omnibus** code proper, you must run the routine `omni_glimpse_starsub.pro`.  The star-subtraction process takes a considerable amount of time to run, so please plan accordingly.



##### Ancillary Data 

Because **distance-omnibus** estimates the distance to dense molecular cloud structures in the Milky Way based in part on ancillary data, the following data sets are required:
* The *Spitzer*/GLIMPSE mid-infrared survey V3.5 mosaics (available for the [GLIMPSE I](http://irsa.ipac.caltech.edu/data/SPITZER/GLIMPSE/images/I/1.2_mosaics_v3.5/) and [GLIMPSE II](http://irsa.ipac.caltech.edu/data/SPITZER/GLIMPSE/images/II/1.2_mosaics_v3.5/) coverage regions).  Specifically required are the Band 1 and Band 4 images (`*_I1.fits` and `*_I4.fits`).
* The BU-FCRAO Galactic Ring Survey <sup>13</sup>CO(1-0) data cubes (available [here](http://grunt.bu.edu/grs-stitch/download-all.php)).  The code assumes you have all the cubes to avoid edge effects.


=======
### Release Information

Pre-release version [v0.9.7](https://github.com/BGPS/distance-omnibus/archive/v0.9.7.tar.gz) available as of 11/11/14.
