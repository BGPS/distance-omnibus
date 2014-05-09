distance-omnibus
================

### Description

This is the source repository for the Bolocam Galactic Plane Survey (BGPS) effort to resolve distance measurements to catalog sources. Through the Bayesian application of prior Distance Probability Density Functions (DPDFs) derived from ancillary data to a kinematic distance likelihood, this code derives posterior DPDFs for catalog sources. This methodology is generalized for use with any (sub-)millimeter survey of the Galactic plane. 

The methodology was introduced in [Ellsworth-Bowers et al. (2013, ApJ, 770, 39)](http://adsabs.harvard.edu/abs/2013ApJ...770...39E) and demonstrated on the BGPS version 1 data [(Aguirre et al. 2011, ApJS, 192, 4)](http://adsabs.harvard.edu/abs/2011ApJS..192....4A).  An expansion of the distance methodology to include a new kinematic distance likelihood and prior DPDFs is presented in Ellsworth-Bowers et al. (2014, ApJ, vvv, ppp), and demonstrated on the re-reduced BGPS version 2 data of [Ginsburg et al. (2013, ApJS, 208, 14)](http://adsabs.harvard.edu/abs/2013ApJS..208...14G).

=======
### Software Requirements

This package is written entirely in the [Interactive Data Language (IDL)](http://www.exelisvis.com/ProductsServices/IDL.aspx), and requires a recent version (8.0 or higher) to run.

Several external libraries of IDL routines are also required to run **distance-omnibus**.  These libraries must be installed on the local machine and their paths included in the IDL path.  The **distance-omnibus** code assumes you have a version of these libraries *no older* than the release date shown below.
   * IDLASTRO (http://idlastro.gsfc.nasa.gov/) or (https://github.com/wlandsman/IDLAstro)
   * The Coyote Graphics System (http://www.idlcoyote.com/idldoc/cg/index.html) or (https://code.google.com/p/idl-coyote/)
   * The Markwardt IDL Library (http://www.physics.wisc.edu/~craigm/idl/)


### Ancillary Data Requirements

Because **distance-omnibus** estimates the distance to dense molecular cloud structures in the Milky Way based in part on ancillary data, the following data sets are required:
* The *Spitzer*/GLIMPSE mid-infrared survey V3.5 mosaics (available for the [GLIMPSE I](http://irsa.ipac.caltech.edu/data/SPITZER/GLIMPSE/images/I/1.2_mosaics_v3.5/) and [GLIMPSE II](http://irsa.ipac.caltech.edu/data/SPITZER/GLIMPSE/images/II/1.2_mosaics_v3.5/) coverage regions).  Specifically required are the Band 1 and Band 4 images (`*_I1.fits` and `*_I4.fits`).
* The BU-FCRAO Galactic Ring Survey <sup>13</sup>CO(1-0) data cubes (available [here](http://grunt.bu.edu/grs-stitch/download-all.php)).  The code assumes you have all the cubes to avoid edge effects.



=======
### Release Information

Pre-release version [v0.9.2](https://github.com/BGPS/distance-omnibus/archive/v0.9.2.tar.gz) available as of 5/9/14.
