# Changelog

## pavo 2.10.0

### NEW FEATURES AND SIGNIFICANT CHANGES

- the `rimg2cimg()` function has been removed in favour of a custom
  `as.cimg()` method.
- [`jndrot()`](https://pavo.colrverse.com/reference/jndrot.md) and by
  extension
  [`jnd2xyz()`](https://pavo.colrverse.com/reference/jnd2xyz.md) have
  been adjusted for trichromats to only allow rotations in the 2D plane.
  Until now, 3D rotations were allowed and the result was projected back
  in 2D but this meant that the output was no longer representing JNDs
  distances. Because `rotate = TRUE` is the default in
  [`jnd2xyz()`](https://pavo.colrverse.com/reference/jnd2xyz.md), we
  recommend you re-run any
  [`jnd2xyz()`](https://pavo.colrverse.com/reference/jnd2xyz.md)
  computation on trichromats. From our tests, results stay qualitatively
  similar but specific values may change.

### MINOR FEATURES AND BUG FIXES

- default values for some arguments in
  [`explorespec()`](https://pavo.colrverse.com/reference/explorespec.md)
  and [`sensmodel()`](https://pavo.colrverse.com/reference/sensmodel.md)
  are now explicit in the function definition.
- [`as.rspec()`](https://pavo.colrverse.com/reference/as.rspec.md) now
  works out of the box with `tibble`, rather than requiring users to
  pass a standard data.frame.
- argument checks in
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md)
  internal functions
  ([`dispace()`](https://pavo.colrverse.com/reference/dispace.md),
  [`trispace()`](https://pavo.colrverse.com/reference/trispace.md),
  [`hexagon()`](https://pavo.colrverse.com/reference/hexagon.md), etc.)
  have been refactored
  ([\#263](https://github.com/rmaia/pavo/issues/263)), with two minor
  user-facing consequences:
  - some messages in
    [`tcspace()`](https://pavo.colrverse.com/reference/tcspace.md) have
    been converted to warnings for consistency with the other internal
    colspace functions
  - some error messages are been reorder, which means that errors
    produced for inputs invalid for multiple reasons may appear in a
    different order than in previous versions.
- pavo now uses R 4.1 (released in 2021) as the minimum required R
  version.
- the values of `avg.uv` and `avg.v` visual systems used in
  [`sensdata()`](https://pavo.colrverse.com/reference/sensdata.md) and
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) have
  been updated to match exactly the ones provided in the original source
  (Endler & Mielke 2005). This may result in minute changes in the
  values returned by
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) if
  you are using these parameters.

### INTERNAL CHANGES

- dependency on the `viridisLite` package has been removed in favour of
  recent base R functions

## pavo 2.9.0

CRAN release: 2023-09-24

### NEW FEATURES AND SIGNIFICANT CHANGES

- Added a new function
  [`simulate_spec()`](https://pavo.colrverse.com/reference/simulate_spec.md),
  which allows for the flexible simulation of naturalistic spectra (inc.
  reflectance, irradiance, radiance, absorbance). See
  [`?simulate_spec`](https://pavo.colrverse.com/reference/simulate_spec.md)
  for examples and information, and the handbook for further discussion.
- [`plot.rspec()`](https://pavo.colrverse.com/reference/plot.rspec.md)
  now accepts a logical `labels` argument (and `labels.cex`), to control
  whether text labels identifying each spectrum should be added to the
  outer plot margins. This was previously only available, and was
  required, for ‘stacked’ plot types, but is now optional for both
  ‘overlay’ (the default) and ‘stacked’ spectral plots.
- the `wlmin` and `wlmax` arguments in
  [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md)
  are being deprecated in favour of a single `lim` argument, for
  consistency across functions.

### MINOR FEATURES AND BUG FIXES

- [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md)
  has been rewritten for efficiency, and now only calculates the
  required variables when `subset` is used. As a result, the function is
  also slightly slower (0.5 x) when calculating the full set of
  variables, but much faster (10 x) when calculating a subset.
- Removed the start-up message.
- Removed the previously-deprecated `margins` argument from various
  colourspace plots.
- Replace `rgl.triangles` with
  [`triangles3d()`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html)
  internally to avoid a deprecation issue.
- Fixed a bug in the categorical colourspace, so it now returns sensible
  category values when `x` and/or `y` is equal to 0.

## pavo 2.8.0

CRAN release: 2022-08-16

### MINOR FEATURES AND BUG FIXES

- Updated documentation to fix broken HTML5 equation rendering
- Line width in the legend of
  [`aggplot()`](https://pavo.colrverse.com/reference/aggplot.md) (as
  specified by `lwd`) is now consistent with line width on the plot
  ([\#235](https://github.com/rmaia/pavo/issues/235))
- `procspec(spec, opt = "smooth")` now works correctly with
  uninterpolated spectra. Previously, it was causing a shift of the
  spectra towards long-wavelengths
  ([\#234](https://github.com/rmaia/pavo/issues/234)).
- [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) now
  returns Euclidean distances when given data from the `ciexyz` model.
- [`as.rspec()`](https://pavo.colrverse.com/reference/as.rspec.md) now
  preserves numeric column names when converting from a tibble.

## pavo 2.7.1

CRAN release: 2021-09-21

### MINOR FEATURES AND BUG FIXES

- Minor release to avoid breaking changes to tests.

## pavo 2.7.0

CRAN release: 2021-03-23

### NEW FEATURES AND SIGNIFICANT CHANGES

- It is now possible to filter images according to the AcuityView 2.0
  algorithm (Caves et al. 2018) in order to to model the visual acuity
  of animals, prior to analysis. The functionality is implemented in
  [`procimg()`](https://pavo.colrverse.com/reference/procimg.md), with
  details and examples available in the function’s help documentation
  and the vignette.
- Added the argument `raw` to
  [`bootcoldist()`](https://pavo.colrverse.com/reference/bootcoldist.md).
  When `TRUE`, the full raw list of bootstrapped colour-distances are
  returned (equal in length to the number of replicates specified by
  `boot.n`), rather than the summary statistics which are returned by
  default.
- Added the visual phenotype of the Vinegar fly *Drosophila
  melanogaster* (Sharkey et al. 2020). See `?vismodel()` or
  `?sensdata()` for instructions on selecting it.
- [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) now
  uses the package `farver` when calculating CIE2000 colour distances
  between points in the CIELab and CIELch spaces.

### MINOR FEATURES AND BUG FIXES

- Fixed a bug where procimg() would fail when using resize/rotate
  followed by further options, on single images.
- vignettes have been moved to <https://book.colrverse.com/> and now use
  a book format
- Fixed a bug in `plot.colspace(gamut = TRUE)` (as well as
  [`triplot()`](https://pavo.colrverse.com/reference/triplot.md),
  [`tcsplot()`](https://pavo.colrverse.com/reference/tcsplot.md),
  [`tetraplot()`](https://pavo.colrverse.com/reference/tetraplot.md))
  where it would incorrectly display `"Max gamut cannot be plotted."`
  and fail to plot the max gamut each time.
- Fixed a recent bug whereby the D65 illuminant was expressed in units
  of radiant flux, by default. It has instead been converted to photon
  flux (umol.s-1.m-2).
- Added an informative error message when raw quantum catch estimates
  are \< 1, and an attempt is made to calculate colour distances in the
  receptor-noise model using `noise = 'quantum'`. Taking the root of
  negative values following log-transformation, as required when
  `noise = 'quantum'`, would previously produce an uninformative error,
  which has been rectified.
- The `margins` argument has been deprecated for cie, coc, hexagon,
  segment, projplot, di-, tri-, and tetra-chromatic plots. It was
  causing problems when later adding points to plots, and
  margin-handling is best left to the user anyway. The default plots may
  therefore look slightly different, but the margins can be adjusted to
  taste using the standard `par(mar = c())` pathway.
- Fixed a bug where
  [`bootcoldist()`](https://pavo.colrverse.com/reference/bootcoldist.md)
  would not run until supplied with receptor-noise model arguments (n,
  weber, weber.ref), even when not running the receptor-noise model.
- Fixed a bug where
  [`bootcoldist()`](https://pavo.colrverse.com/reference/bootcoldist.md)
  would sometimes fail when given data from colourspace models
  containing lots of negative coordinate values. This was particularly
  common for the hexagon model.
- Improved safety and error handling in
  [`adjacent()`](https://pavo.colrverse.com/reference/adjacent.md).

## pavo 2.6.1

CRAN release: 2020-12-18

### MINOR FEATURES AND BUG FIXES

- Minor fix to a vignette to avoid installation issues.
- [`options()`](https://rdrr.io/r/base/options.html) and
  [`par()`](https://rdrr.io/r/graphics/par.html) are now always set
  locally, including in vignettes and examples, as to prevent spillover
  of these changes in the user session

## pavo 2.6.0

CRAN release: 2020-12-09

### MINOR FEATURES AND BUG FIXES

- [`bootcoldist()`](https://pavo.colrverse.com/reference/bootcoldist.md)
  and [`adjacent()`](https://pavo.colrverse.com/reference/adjacent.md)
  now use a random number generator that generates statistically sound
  values, even when ran in parallel. The output of these functions is
  thus expected to slightly change, even if you set the seed before.
- The alphashape3d package has been moved to `Suggests`, which means it
  will not be installed automatically when you install pavo from CRAN
  and that you will need to install it yourself if you need it (for
  `vol(type = "alpha")`, `vol(type = "alpha")` and
  `voloverlap(type = "alpha")`)

## pavo 2.5.0

CRAN release: 2020-11-12

### NEW FEATURES AND SIGNIFICANT CHANGES

- Add ability to compute colour volume by using alphashapes instead of
  convex hulls. The functions
  [`vol()`](https://pavo.colrverse.com/reference/vol.md),
  [`tcsvol()`](https://pavo.colrverse.com/reference/tcsplot.md) and
  [`voloverlap()`](https://pavo.colrverse.com/reference/voloverlap.md)
  gain a new argument `type = c("convex", "alpha")` to decide how you
  want to compute the colour volume. Please refer to the vignette
  `vignette("pavo-5-alphashapes", package = "pavo")` for more
  information. As a result, the argument order in these 3 function has
  changed. Check the documentation to update your scripts accordingly.
  The function
  [`summary.colspace()`](https://pavo.colrverse.com/reference/summary.colspace.md)
  also gains an additional column that returns that colour volume
  computed with an alpha-shape of parameter alpha\* in the case of `tcs`
  objects.

### MINOR FEATURES AND BUG FIXES

- [`getimg()`](https://pavo.colrverse.com/reference/getimg.md) now
  imports image files with uppercase extensions (e.g., JPG or PNG), such
  as those produced by some camera brands or processing software.
- Maximum quantum catches computation (`data.maxqcatches` attribute) now
  works for segment “visual model” as well. As a side effect, this
  removes a warning that occurred when users ran
  `vismodel(..., visual = "segment")`.
- [`sensmodel()`](https://pavo.colrverse.com/reference/sensmodel.md) now
  accepts the argument `sensnames`, for specifying the names of the
  resulting sensitivity curves on-the-fly.
- CIE models now accept data created outside of
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md), by
  allowing users to specify the illuminant and viewer sensitivity
  function used when estimating XYZ values (via `illum` and `visual`
  arguments in
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md)).
- [`bootcoldist()`](https://pavo.colrverse.com/reference/bootcoldist.md)
  is now laxer in its argument checks and accept objects that are
  neither `vismodel` or `colspace` objects. This means you can now use
  this function on quantum catches dataframe that you obtained outside
  of pavo, such as the MICA toolbox.
- [`summary.colspace()`](https://pavo.colrverse.com/reference/summary.colspace.md)
  now prints a more explicit error when the `by` argument value is not a
  multiple of the number of rows in the colspace object (i.e., the
  number of spectra)
- Added a continuous measure of hue to the output of the categorical
  model of Troje (1993)
- `teal` example dataset columns have been renamed to add an additional
  zero in front of single digit numbers, so that column names now sort
  in the correct order by default.
- Some very small negative values in the built-in visual system data
  have been corrected .
- [`as.rspec()`](https://pavo.colrverse.com/reference/as.rspec.md) is
  now more lenient for wavelength trimming when `interp = FALSE` and now
  works even if the specified `lims` do not correspond to actual wl
  values from the input object.

## pavo 2.4.0

CRAN release: 2020-02-08

### NEW FEATURES AND SIGNIFICANT CHANGES

- Fixed a bug introduced in version 2.3.0 that gave wrong values for
  S1UV and S1V in
  [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md).
- several [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  functions for colspace
  ([`triplot()`](https://pavo.colrverse.com/reference/triplot.md),
  [`tcsplot()`](https://pavo.colrverse.com/reference/tcsplot.md),
  [`tetraplot()`](https://pavo.colrverse.com/reference/tetraplot.md))
  gain a new `gamut` argument to plot the maximum gamut for a given
  visual system and illuminant.
  [`summary.colspace()`](https://pavo.colrverse.com/reference/summary.colspace.md)
  also now returns the maximum colour volume for a given visual system
  and illuminant that you can use to compare to the realised volume by a
  given dataset. More information in PR
  [\#180](https://github.com/rmaia/pavo/issues/180).
- parallel processing now relies on the `future` package, which offers
  windows and high performance computing (HPC) environments support. The
  progress bar is produced by the `progressr` package and can be
  customised as well. As a consequence, the `cores` argument in
  [`getspec()`](https://pavo.colrverse.com/reference/getspec.md),
  [`adjacent()`](https://pavo.colrverse.com/reference/adjacent.md) and
  [`classify()`](https://pavo.colrverse.com/reference/classify.md) has
  been deprecated.

### MINOR FEATURES AND BUG FIXES

- fixed a plotting bug introduced in version 2.3.0 where it was required
  to run
  [`projplot()`](https://pavo.colrverse.com/reference/projplot.md) twice
  for the background grid to be displayed.
- fixed a bug in
  [`summary.colspace()`](https://pavo.colrverse.com/reference/summary.colspace.md)
  where `NULL` was returned instead of
  [`summary.data.frame()`](https://rdrr.io/r/base/summary.html) for
  non-tcs colourspaces.
- fix partial matching warnings in examples and in `bootcooldist()`
- the package has a new website at `pavo.colrverse.com`
- fixed a bug in
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md)that
  prevented the calculation of achromatic contrast when using custom
  quantum catch data

## pavo 2.3.0

CRAN release: 2019-12-11

### NEW FEATURES AND SIGNIFICANT CHANGES

- [`plot.rspec()`](https://pavo.colrverse.com/reference/plot.rspec.md)
  now adds a linear spectrum alongside the x-axis to show the hues
  corresponding to each wavelength (controlled by the `wl.guide` boolean
  argument).
- [`cieplot()`](https://pavo.colrverse.com/reference/cieplot.md) (and
  therefore
  [`plot.colspace()`](https://pavo.colrverse.com/reference/plot.colspace.md)
  for CIEXYZ model) now adds the background of the CIEXYZ colour space
  by default. This can be turned off by switching the `ciebg` argument
  to `FALSE`.
- [`voloverlap()`](https://pavo.colrverse.com/reference/voloverlap.md)
  uses a different algorithm to determine volume overlaps, which means:
  - computation is now much more efficient
  - [`voloverlap()`](https://pavo.colrverse.com/reference/voloverlap.md)
    now works for
    [`trispace()`](https://pavo.colrverse.com/reference/trispace.md)
    objects as well
  - accordingly, first two arguments of
    [`voloverlap()`](https://pavo.colrverse.com/reference/voloverlap.md)
    has been renamed `colsp1` and `colsp2` instead of `tcsres1` and
    `tcsres2`
  - slow and possibly inaccurate `montecarlo` option has been deprecated

### MINOR FEATURES AND BUG FIXES

- errors during argument checks in
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) now
  have more explicit messages.
- all `pavo` functions (excepted
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) and
  [`spec2rgb()`](https://pavo.colrverse.com/reference/spec2rgb.md)) now
  work with sub-nm precision, for `rspec` objects with non-integer
  wavelengths.
- [`summary.colspace()`](https://pavo.colrverse.com/reference/summary.colspace.md)
  no longer fails for tcs objects with only one row.
- [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) now
  works for monochromats as well.
- `procspec(opt = "bin")` no longer counts bin edges twice (once in each
  consecutive bin). This will cause changes in the bin stops compared to
  earlier versions of this function.
- [`coc()`](https://pavo.colrverse.com/reference/coc.md) and
  [`categorical()`](https://pavo.colrverse.com/reference/categorical.md)
  spaces now return Weber luminance contrast by default when passed
  through
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md), rather
  than nothing (as per the original publications).
- [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) can now
  take multiple values for `weber`, when Weber fractions are known for
  all receptor classes
- [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md)
  no longer errors for a single spectrum when the wavelength range does
  not contain 450-700nm

## pavo 2.2.0

CRAN release: 2019-06-25

### NEW FEATURES AND SIGNIFICANT CHANGES

- added k-medoids clustering as an option in
  [`classify()`](https://pavo.colrverse.com/reference/classify.md)
- [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) now
  returns unweighted Euclidean distances for generic di-, tri-, and
  tetra-chromatic
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md)
  models. Users wishing to estimate noise-weighted distances (i.e. via
  the receptor-noise limited model) should continue to use
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md)
  directly on
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md)
  objects, as previously.

### MINOR FEATURES AND BUG FIXES

- added a message to indicate what distance metric is being calculated
  during calls to
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md)
- [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md),
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md), and
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) now
  always return `lum` and `dL` columns, for consistency
- [`irrad2flux()`](https://pavo.colrverse.com/reference/irrad2flux.md)
  and
  [`flux2irrad()`](https://pavo.colrverse.com/reference/irrad2flux.md)
  use slightly more precise constant values, which may lead to slightly
  different results (less than 0.1% difference between this new version
  and the previous versions)
- [`getimg()`](https://pavo.colrverse.com/reference/getimg.md) can now
  import images from URL’s
- the `cores` argument in
  [`getimg()`](https://pavo.colrverse.com/reference/getimg.md) is
  deprecated, as image import is vectorised
- fixed a small bug in
  [`classify()`](https://pavo.colrverse.com/reference/classify.md) when
  using `refID` and `kcols` for multiple images
- the `resize` argument in
  [`procimg()`](https://pavo.colrverse.com/reference/procimg.md) now
  takes a percentage, rather than scaling factor
- fixed a bug where `colspace(space = segment)` would return both a `B`
  and (redundant) `lum` column
- fixed a bug where the rod sensitivity of *Canis familiaris* was
  inaccessible through
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md)
- fixed an issue in
  [`getspec()`](https://pavo.colrverse.com/reference/getspec.md) where
  badly encoded characters in some spectral files would cause failure
- fixed a bug where
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) would
  attempt to estimate receptor-noise weighted distances rather than
  cie-distances for `cielch` model results

## pavo 2.1.0

CRAN release: 2019-03-06

### NEW FEATURES AND POTENTIALLY BREAKING CHANGES

- added the argument `reclass` to
  [`procimg()`](https://pavo.colrverse.com/reference/procimg.md), which
  allows users to interactively correct areas within images that have
  been misclassified
- added the rod sensitivity of *Canis familiaris*
- [`peakshape()`](https://pavo.colrverse.com/reference/peakshape.md)
  uses a completely different algorithm to find the FWHM. It now works
  as expected for spectra with multiple peaks. See
  [PR](https://github.com/rmaia/pavo/pull/137)
  [\#137](https://github.com/rmaia/pavo/issues/137) for a detailed
  overview of the changes.
- data used internally by pavo (`bgandilum`, `transmissiondata`,
  `ttvertex`, `vissyst`) is no longer exposed to users

### MINOR FEATURES AND BUG FIXES

- new functions
  [`is.vismodel()`](https://pavo.colrverse.com/reference/is.vismodel.md)
  and
  [`is.colspace()`](https://pavo.colrverse.com/reference/is.colspace.md)
  are exported to test whether an object is of class `vismodel` or
  `colspace`, respectively
- fixed a bug where images would sometimes be wrongly detected as
  user-classified in
  [`as.rimg()`](https://pavo.colrverse.com/reference/as.rimg.md)
- the UV-sensitive cone is now only always named “u”, even for VS
  species (such as `pfowl` and `avg.v` in
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) and
  [`sensdata()`](https://pavo.colrverse.com/reference/sensdata.md)).
  This removes an unnecessary but harmless warning when
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md) was
  used to place quantum catches of such species in the tetrahedral
  colour space.
- the `achro` argument in
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) has
  been changed for `achromatic` to better match the arguments from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md).
  Older scripts that use `achro` should not be affected and still work
  as before.
- the package `imager` is no longer a dependency, and is only loaded if
  using some features of
  [`procimg()`](https://pavo.colrverse.com/reference/procimg.md).
- the package `mapproj` is no longer a dependency, and is only loaded if
  using
  [`projplot()`](https://pavo.colrverse.com/reference/projplot.md).
- added the argument `labels.stack` to `plot.rspec`, which allows the
  use of custom spectra labels in stacked plots.
- users now receive a warning when interpolating beyond the limits of
  the data using `as.rspec`, and can control the behaviour with the new
  argument `exceed.limits`.
- all deprecated functions and arguments have now been fully removed.
- [`as.rspec()`](https://pavo.colrverse.com/reference/as.rspec.md) now
  accepts both numeric and character vectors to identify the wavelength
  column using `whichwl` (eg. `whichwl = "wl"`).  
- Reference images in
  [`classify()`](https://pavo.colrverse.com/reference/classify.md) can
  now be specified using either a numeric vector (to identify by image
  position in a list) or character vector (to identify by image name).  
- fixed a bug in
  [`aggspec()`](https://pavo.colrverse.com/reference/aggspec.md) when
  wavelength column was previously removed by the user.
- fixed a bug where
  [`cocplot()`](https://pavo.colrverse.com/reference/cocplot.md) would
  failed whenever `type` graphical parameter was specified.
- [`spec2rgb()`](https://pavo.colrverse.com/reference/spec2rgb.md) has
  been simplified to rely more on
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md). As a
  result, output values may be slightly different but upon testing, we
  found that differences between the old and the new version were barely
  noticeable.
- the vignette have been split into three smaller parts, which should
  help new users to get started with pavo
- numerous under-the-hood changes for stability and speed, with thanks
  to three reviewers and an associate editor at MEE.

## pavo 2.0.0

CRAN release: 2018-09-22

### NEW FEATURES

- image-based workflow for the combined analysis of colour and pattern
  geometry
- added the visual systems of the (trichromatic) jumping spider
  *Habronattus pyrrithrix* and the (trichromatic) triggerfish
  *Rhinecanthus aculeatus*
- [`getspec()`](https://pavo.colrverse.com/reference/getspec.md) can now
  read Avantes binary files (`.TRM` files)

### MINOR FEATURES AND BUG FIXES

- Carotenoid chroma (S9) in
  [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md)
  has been fixed to (R700 - R450)/R700. This gives the same result as
  before but with a flipped sign, and better reflects the original
  formula in the literature.
- cieLAB values have been rescaled, and are expressed in the more
  standard range: L \[0,100\], ab \[-128,127\]
- [`getspec()`](https://pavo.colrverse.com/reference/getspec.md) has an
  additional argument `ignore.case` set to `TRUE` by default to ignore
  case in file extension matching
- fix a bug where
  [`getspec()`](https://pavo.colrverse.com/reference/getspec.md) would
  sometimes fail with files including numbers in scientific format
- add a new option in
  [`tetraplot()`](https://pavo.colrverse.com/reference/tetraplot.md) to
  add cone names (u,s,m,l)

## pavo 1.4.0

CRAN release: 2018-07-11

### NEW FEATURES

- [`getspec()`](https://pavo.colrverse.com/reference/getspec.md) can now
  read OceanOptics `.ProcSpec` files
- added the visual system of *Ctenophorus ornatus*, the (trichromatic)
  ornate dragon lizard

### MAJOR CHANGES

- `getspecf()` (and the argument `fast = TRUE` in
  [`getspec()`](https://pavo.colrverse.com/reference/getspec.md)) have
  been deprecated
- [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md)
  returned incorrect values for S7. If you use S7, please re-run your
  analyses

### MINOR FEATURES AND BUG FIXES

- [`summary.rspec()`](https://pavo.colrverse.com/reference/summary.rspec.md)
  now properly outputs `NA` for monotonically decreasing spectra
- fixed warning when
  [`subset.rspec()`](https://pavo.colrverse.com/reference/subset.rspec.md)
  was provided with a logical vector
- fixed harmless warning when
  [`summary.colspace()`](https://pavo.colrverse.com/reference/summary.colspace.md)
  was used on a tcs object
- `by` argument in
  [`merge.rspec()`](https://pavo.colrverse.com/reference/merge.rspec.md)
  is no longer ignored
- fixed bug in
  [`voloverlap()`](https://pavo.colrverse.com/reference/voloverlap.md)
  when plot = TRUE
- fixed bug in
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) when
  transmission has more than one column
- fixed bug in
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) that
  applied von Kries correction to achromatic channel
- added argument `fill=FALSE` in
  [`voloverlap()`](https://pavo.colrverse.com/reference/voloverlap.md)
- fixed bug in
  [`jndplot()`](https://pavo.colrverse.com/reference/jndplot.md) when
  suppressing the plotting of arrows
- better handling of subset data when using summary.colspace() and
  summary.vismodel()
- fixed bug in
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) when
  `noise = "quantum"` and `achro = TRUE` were used
- fixed bug in
  [`jndplot()`](https://pavo.colrverse.com/reference/jndplot.md) when
  `arrow = "none"` and `achro = TRUE`
- [`spec2rgb()`](https://pavo.colrverse.com/reference/spec2rgb.md) now
  takes into account the 390-400 nm wavelength range into account when
  possible
- [`as.rspec()`](https://pavo.colrverse.com/reference/as.rspec.md) no
  longer fails with tibbles
- bin option of
  [`procspec()`](https://pavo.colrverse.com/reference/procspec.md) now
  works for all values of bins
- non-relative quantum catches from dataframe object were not correctly
  scaled in “di”, “tri”, “categorical” and “coc” colorspaces
- fixed a bug in
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md) where
  it would incorrectly infer a preference for a general trichromatic
  space, when a cie model is more appropriate
- fixed a bug so that cie color matching functions can be more easily be
  used in a general trichromatic space (i.e. maxwell triangle)

## pavo 1.3.1

CRAN release: 2017-10-02

### NEW FUNCTIONS

- [`bootcoldist()`](https://pavo.colrverse.com/reference/bootcoldist.md)
  uses bootstrap to calculate the confidence intervals on the mean color
  distance between samples

### MINOR FEATURES AND BUG FIXES

- fixed bug in
  [`coldist()`](https://pavo.colrverse.com/reference/coldist.md) when
  fewer samples than the number of photoreceptors
- fixed bug in
  [`getspec()`](https://pavo.colrverse.com/reference/getspec.md) that
  would cause it to crash with errors
- fixed bug in
  [`tetraplot()`](https://pavo.colrverse.com/reference/tetraplot.md)
  when saving images

## pavo 1.3.0

CRAN release: 2017-09-20

### NEW FUNCTIONS

- jnd2xyz() converts distances (in JND, resulting from a coldist() call)
  into cartesian coordinates
- plot() methods for objects resulting from jnd2xyz()
- jndrot() produces rotations of Cartesian coordinates resulting from
  jnd2xyz()
- coldist2mat() converts coldist() result from a pairwise data.frame to
  a distance matrix
- sensdata() function for retrieving and/or visualising pavo’s in-build
  spectral data

### MAJOR CHANGES

- tetraplot() and cieplot() have been completely rewritten to allow
  finer viewing control
- tetraplot() allows forced perspective using size to denote distance
- voloverlap() and vol() have also been changed to work with the new
  tetraplot() options
- getspec() has been rewritten to be faster, more general, and allow
  parallel processing
- subset functions now allow more than one argument to be used, and
  allow further attributes to be passed onto grep (e.g. invert = TRUE)

### MINOR FEATURES AND BUG FIXES

- fixed bug in coldist() on log-transformation when object was neither
  of class vismodel nor colspace
- fixed bug in dL calculation when input is a colspace object
- fixed bug in vismodel() when a data frame, matrix or rspec object was
  passed as the background
- fixed bug in colspace() models when using non-standard receptor names
  or ordering
- fixed bug in hexagon() model when calculating location & metrics for
  achromatic stimuli
- fixed location of red vertex in tetraplot()
- fixed bug in the argument names for expanding text labels in colspace
  plots
- removed na.rm argument from aggspec() that was causing a bug when the
  error function did not have that argument. User should pass it as an
  argument to the function if necessary.
- changed default to achro=FALSE in coldist()
- replaced the modelled receptor sensitivities of the honeybee *Apis
  mellifera* with the empirical sensitivities from Peitsch et al (1992)
- the built-in ‘green’ background spectrum is no longer normalized
- removed wavelength limitations in the calculation of H3 from
  summary.rspec
- all visual systems (except CIE) have been normalized to have an
  integral of 1

## pavo 1.2.0

### MAJOR CHANGES

- added the CIELch model accessed via colspace(space = ‘cielch’)
- added the sensdata() function for retrieving and/or visualising pavo’s
  in-build spectral data

### MINOR FEATURES AND BUG FIXES

- vignettes have been amalgamated & the single, main vignette is now
  up-to-date
- added more informative labels for the segment analysis plot

## pavo 1.1.0

CRAN release: 2017-05-05

### NEW FUNCTIONS

- segspace() replaces the deprecated segclass(), and is accessed via the
  colspace() argument space = ‘segment’. The results of segspace() are
  also now compatible with coldist() for the estimation of Euclidean
  colour-distances.
- segplot() is a plot for Endler’s (1990) segment analysis, and is
  accessed — along with all other 2d plots — via plot.colspace()

### MINOR FEATURES AND BUG FIXES

- the use of relative quantum catches is now optional in the categorical
  colorspace (though still produces a warning), for greater flexibility
- updated several functions to work when rspec object has only one
  spectrum
- fixed bug in voloverlap where interactive plots would result in error
- fixed incorrect labels in the maxwell triangle plot
- fixed a bug in as.rspec() in which lim was not applied when
  interpolate = FALSE
- fixed bug in aggplot() which resulted in error when using lty, lwd
  arguments
- warning if ocular media is being used in both vismodel() and
  sensmodel()
- added an ‘all’ option to the achromatic argument in vismodel()
- added the ability to calculate dL for cielab models in coldist()
- added some more informative messages and warnings

## pavo 1.0

- See vignette for detailed description of changes.

### MAJOR CHANGES

- coldist() arguments have been changed. Now the empirically estimated
  value for the Weber fraction must be entered, instead of the
  noise-to-signal ratio. The noise-to-signal ratio is then calculated
  based on the empirically estimated Weber fraction for the reference
  cone type, and applied to the remaining cone types. This should avoid
  confusion between empirically estimated values for the Weber fraction
  and the noise-to-signal ratio, which are currently prevalent in the
  literature.
- coldist() now has an additional argument, weber.achro, so that the
  value for the Weber fraction to be used to calculate achromatic
  contrast can be input independently of the cone ratios.
- tcs() is deprecated, replaced by colspace().

### NEW FUNCTIONS

- colspace() replaces tcs() and introduces several new colorspaces
- plot() methods for several colspace() outputs, including a static
  tetrahedral colorspace
- projpoints() allows the plotting of points in a projplot() figure
- vol() draws volume polygons in static tetrahedral plots
- axistetra() draws reference x, y and z axis arrows in static
  tetrahedral plots
- legendtetra() adds legends to static tetrahedral plots

### MINOR FEATURES AND BUG FIXES

- summary.colspace() for tcs spaces now returns relative color volume as
  well as absolute
- tcsvol() and voloverlap() now allow control for line width
- procspec() fixed error when attempting to smooth rspec object without
  column names
- procspec() handles smoothing before fixing negatives to avoid
  re-adding negatives when smoothing
- procspec(), aggplot() accept additional arguments to summary functions
  (e.g., na.rm=TRUE)
- peakshape() default wavelength limits (lim) now taken from rspec
  object rather than 300-700
- peakshape() returns warning if a spectrum contains duplicate
  reflectance values
- summary.rspec() works with single spectra
- aggplot() and aggspec() fixed bug on ordering of levels when they
  don’t match the sequence in the rspec object
- aggplot() added logical argument “legend” for automatically adding a
  legend to the plot
- vismodel() returns error if bkg=NULL
- getspec() patched to stop returning warnings in Yosemite
- getspec() has a faster (~5-10X), but less flexible, algorithm used
  when all input files are from the same source.

## pavo 0.5-6

CRAN release: 2016-10-20

### BUG FIXES

- fixed bug in calculation of dichromat contrast in coldist()

## pavo 0.5-5

CRAN release: 2016-03-09

### MINOR FEATURES AND BUG FIXES

- fixed bug in calculation of H3 in summary.rspec()

## pavo 0.5-4

CRAN release: 2016-01-28

### MINOR FEATURES AND BUG FIXES

- changed default values for coldist()

## pavo 0.5-2

CRAN release: 2015-06-11

### MINOR FEATURES AND BUG FIXES

- fixes to the blue tit visual system, changed vismodel() argument to
  “bluetit”

## pavo 0.5-1

CRAN release: 2013-09-04

### MINOR FEATURES AND BUG FIXES

- vismodel() accepts matrix, data.frame or rspec objects for the
  illuminant, updated warning messages associated with this use
- vismodel() accepts user-defined achromatic receptors
- tcsplot(), tcsvol() & tcspoints(): transparency control passed to user
- getspec() works with OceanView files

## pavo 0.5

CRAN release: 2013-06-09

- updated citation()

### MINOR FEATURES AND BUG FIXES

- vismodel() vonkries = TRUE does not return a NULL result
- vismodel() works with a single spectrum object
- tcsplot() allows greater control of tetrahedron appearance
- summary.rspec() allows for user-defined minimum wavelength (for
  calculation of UV variables)

## pavo 0.3-1

CRAN release: 2013-05-06

### NEW FUNCTIONS

- irrad2flux() and flux2irrad() to convert illuminant measurements

### MINOR FEATURES AND BUG FIXES

- vismodel() less cryptic error messages
- as_rspec() fix message pertaining wavelength column
- getspec() now removes empty columns (generated by bad tabulation)
- plot.rspec() fix color labelling issue (previously, when user
  specified fewer colors than number of spectra, ‘stack’ and ‘overlay’
  colored spectra differently)

## pavo 0.3

CRAN release: 2013-04-23

### NEW FUNCTIONS

- subset() class methods for rspec, vismodel and tcs
- summary() method for vismodel, returns attributes used in visual model

### VISUAL MODEL

- vismodel() output includes only Qi or fi (as selected by qcatch
  argument)
- vismodel() von Kries correction is now an optional argument
- coldist() subset argument for partial filtering
- Updated sensitivity curves

### MINOR FEATURES

- getspec() works with Avasoft 8 output
- aggspec() if no “by” argument is supplied, applies function to all
  spectra

### BUG FIXES

- tcs() accepts receptor names other than usml, issues warning
- aggplot() allows control for different line types per spectra
- voloverlap() now assigns darkgrey color for overlap if color vector is
  of length 1 or 2

## pavo 0.1-2

CRAN release: 2013-01-12

### NEW FUNCTIONS

- merge.rspec() combines rspec objects in a single object

### VISUAL MODEL

- Changed vismodel() output: von Kries correction is now an optional
  argument, output includes only Qi and fi
- vismodel() accepts a scale parameter (multiplies illuminant in order
  to make it in flux units)
- coldist() includes option for receptor noise calculation
- Updated sensitivity curves

### MINOR FEATURES

- sensmodel() accepts user-defined ocular media transmission
- voloverlap() allows greater user control of plotting options
- voloverlap() includes Monte Carlo option for overlap calculation
- as.rspec() accepts “whichwl” argument for user-defined wavelength
  column selection
- as.rspec() includes “interp” argument; updated automatic search for
  wavelength column
- aggspec() aggregates spectra by multiple vectors (e.g. average spectra
  by species and sex using by=list(species,sex) )
- peakshape() gives plot titles, allows ask=TRUE and mfrow

### BUG FIXES

- explorespec() “free” argument fixed
- summary.rspec() S5 variable fixed; segments now divided by B1
  (brightness-independent measure of saturation)
- summary.rspec() checks for positive values when bmaxneg is calculated,
  returns NA
- tcs() accepts “v” cone class
- aggspec() fixed matching of names
- procspec() works with rspec objects that include a single reflectance
  spectrum
- peakshape() minor fixes
