pavo 1.3
------------------------------------------------------------------------------
MAJOR CHANGES:

* tetraplot() and cieplot() have been completely rewritten to allow finer viewing control
* getspec() has been rewritten to be faster and more general
* subset functions now allow more than one argument to be used, and allow further attributes to be passed onto grep (e.g. invert = TRUE)

MINOR FEATURES AND BUG FIXES:
* fixed bug in coldist() on log-transformation when object was neither of class vismodel nor colspace
* fixed bug in dL calculation when input is a colspace object
* fixed bug in vismodel() when a data frame, matrix or rspec object was passed as the background
* fixed bug in colspace() models when using non-standard receptor names or ordering
* fixed bug in hexagon() model when calculating location & metrics for achromatic stimuli
* fixed location of red vertex in tetraplot()
* fixed a bug in the argument names for expanding text labels in colspace plots
* removed na.rm argument from aggspec() that was causing a bug when the error function did not have that argument. User should pass it as an argument to the function if necessary.
* changed default to achro=FALSE in coldist() 
* replaced the modelled receptor sensitivities of the honeybee _Apis melifera_ with the empirical sensitivities from Peitsch et al (1992)
* the built-in 'green' background spectrum is no longer normalized
* removed wavelength limitations in the calculation of H3 from summary.rspec

pavo 1.2
------------------------------------------------------------------------------

MAJOR CHANGES:
* added the CIELch model accessed via colspace(space = 'cielch')
* added the sensdata() function for retrieving and/or visualising pavo's in-build spectral data 

MINOR FEATURES AND BUG FIXES:
* vignettes have been amalgamated & the single, main vignette is now up-to-date
* added more informative labels for the segment analysis plot

pavo 1.1
------------------------------------------------------------------------------
MAJOR CHANGES:

NEW FUNCTIONS:
* segspace() replaces the deprecated segclass(), and is accessed via the colspace() argument space = 'segment'. The results of segspace() 
are also now compatible with coldist() for the estimation of Euclidean colour-distances.
* segplot() is a plot for Endler's (1990) segment analysis, and is accessed — along with all other 2d plots — via plot.colspace()

MINOR FEATURES AND BUG FIXES:

* the use of relative quantum catches is now optional in the categorical colorspace (though still produces a warning), for greater flexibility 
* updated several functions to work when rspec object has only one spectrum
* fixed bug in voloverlap where interactive plots would result in error
* fixed incorrect labels in the maxwell triangle plot
* fixed a bug in as.rspec() in which lim was not applied when interpolate = FALSE
* fixed bug in aggplot() which resulted in error when using lty, lwd arguments
* warning if ocular media is being used in both vismodel() and sensmodel()
* added an 'all' option to the achromatic argument in vismodel()
* added the ability to calculate dL for cielab models in coldist()
* added some more informative messages and warnings

pavo 1.0
------------------------------------------------------------------------------
* See vignette for detailed description of changes.

MAJOR CHANGES:
* coldist() arguments have been changed. Now the empirically estimated value for the Weber fraction must be entered, instead of the noise-to-signal ratio. The noise-to-signal ratio is then calculated based on the empirically estimated Weber fraction for the reference cone type, and applied to the remaining cone types. This should avoid confusion between empirically estimated values for the Weber fraction and the noise-to-signal ratio, which are currently prevalent in the literature.
* coldist() now has an additional argument, weber.achro, so that the value for the Weber fraction to be used to calculate achromatic contrast can be input independently of the cone ratios.
* tcs() is deprecated, replaced by colspace().

NEW FUNCTIONS:

* colspace() replaces tcs() and introduces several new colorspaces
* plot() methods for several colspace() outputs, including a static tetrahedral colorspace
* projpoints() allows the plotting of points in a projplot() figure
* vol() draws volume polygons in static tetrahedral plots
* axistetra() draws reference x, y and z axis arrows in static tetrahedral plots
* legendtetra() adds legends to static tetrahedral plots

MINOR FEATURES AND BUG FIXES:

* summary.colspace() for tcs spaces now returns relative color volume as well as absolute
* tcsvol() and voloverlap() now allow control for line width
* procspec() fixed error when attempting to smooth rspec object without column names
* procspec() handles smoothing before fixing negatives to avoid re-adding negatives when smoothing
* procspec(), aggplot() accept additional arguments to summary functions (e.g., na.rm=TRUE)
* peakshape() default wavelength limits (lim) now taken from rspec object rather than 300-700
* peakshape() returns warning if a spectrum contains duplicate reflectance values
* summary.rspec() works with single spectra
* aggplot() and aggspec() fixed bug on ordering of levels when they don't match the sequence in the rspec object
* aggplot() added logical argument "legend" for automatically adding a legend to the plot
* vismodel() returns error if bkg=NULL
* getspec() patched to stop returning warnings in Yosemite
* getspec() has a faster (~5-10X), but less flexible, algorithm used when all input files are from the same source.

pavo 0.5-6
------------------------------------------------------------------------------
BUG FIXES
* fixed bug in calculaiton of dichromat contrast in coldist()

pavo 0.5-5
------------------------------------------------------------------------------
MINOR FEATURES AND BUG FIXES
* fixed bug in calculaiton of H3 in summary.rspec()

pavo 0.5-4
------------------------------------------------------------------------------
MINOR FEATURES AND BUG FIXES
* changed default values for coldist()

pavo 0.5-2
------------------------------------------------------------------------------
MINOR FEATURES AND BUG FIXES
* fixes to the blue tit visual system, changed vismodel() argument to "bluetit"

pavo 0.5-1
------------------------------------------------------------------------------
MINOR FEATURES AND BUG FIXES
* vismodel() accepts matrix, data.frame or rspec objects for the illuminant, updated warning messages associated with this use
* vismodel() accepts user-defined achromatic receptors
* tcsplot(), tcsvol() & tcspoints(): transparency control passed to user
* getspec() works with OceanView files

pavo 0.5
------------------------------------------------------------------------------

* updated citation()

MINOR FEATURES AND BUG FIXES
* vismodel() vonkries = TRUE does not return a NULL result
* vismodel() works with a single spectrum object
* tcsplot() allows greater control of tetrahedron appearance
* summary.rspec() allows for user-defined minimum wavelength (for calculation of UV variables)

pavo 0.3-1
------------------------------------------------------------------------------
NEW FUNCTIONS

* irrad2flux() and flux2irrad() to convert illuminant measurements

MINOR FEATURES AND BUG FIXES

* vismodel() less cryptic error messages
* as_rspec() fix message pertaining wavelength column
* getspec() now removes empty columns (generated by bad tabulation)
* plot.rspec() fix color labeling issue (previously, when user specified fewer colors than number of spectra, 'stack' and 'overlay' colored spectra differently)

pavo 0.3
------------------------------------------------------------------------------
NEW FUNCTIONS

* subset() class methods for rspec, vismodel and tcs
* summary() method for vismodel, returns attributes used in visual model

VISUAL MODEL

* vismodel() output includes only Qi or fi (as selected by qcatch argument)
* vismodel() von Kries correction is now an optional argument
* coldist() subset argument for partial filtering
* Updated sensitivity curves

MINOR FEATURES

* getspec() works with Avasoft 8 output
* aggspec() if no "by" argument is supplied, applies function to all spectra

BUG FIXES

* tcs() accepts receptor names other than usml, issues warning
* aggplot() allows control for different line types per spectra
* voloverlap() now assigns darkgrey color for overlap if color vector is of length 1 or 2

pavo 0.1-2
------------------------------------------------------------------------------
NEW FUNCTIONS

* merge.rspec() combines rspec objects in a single object

VISUAL MODEL

* Changed vismodel() output: von Kries correction is now an optional argument, output includes only Qi and fi
* vismodel() accepts a scale parameter (multiplies iluminant in order to make it in flux units)
* coldist() includes option for receptor noise calculation
* Updated sensitivity curves

MINOR FEATURES

* sensmodel() accepts user-defined ocular media transmission
* voloverlap() allows greater user control of plotting options
* voloverlap() includes Monte Carlo option for overlap calculation
* as.rspec() accepts "whichwl" argument for user-defined wavelength column selection
* as.rspec() includes "interp" argument; updated automatic search for wavelength column
* aggspec() aggregates spectra by multiple vectors (e.g. average spectra by species and sex using by=list(species,sex) )
* peakshape() gives plot titles, allows ask=TRUE and mfrow

BUG FIXES

* explorespec() "free" argument fixed
* summary.rspec() S5 variable fixed; segments now divided by B1 (brightness-independent measure of saturation)
* summary.rspec() checks for positive values when bmaxneg is caluclated, returns NA
* tcs() accepts "v" cone class
* aggspec() fixed matching of names
* procspec() works with rspec objects that include a single reflectance spectrum
* peakshape() minor fixes
