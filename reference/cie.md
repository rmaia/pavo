# CIE colour spaces

Calculates coordinates and colorimetric variables that represent
reflectance spectra in either the CIEXYZ (1931), CIELAB (1971), or
CIELCh (1971) colourspaces.

## Usage

``` r
cie(
  vismodeldata,
  space = c("XYZ", "LAB", "LCh"),
  visual = c("cie2", "cie10"),
  illum = c("D65", "bluesky", "forestshade")
)
```

## Arguments

- vismodeldata:

  (required) quantum catch color data. Can be either the result from
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) or
  independently calculated data (in the form of a data frame with three
  columns representing trichromatic viewer).

- space:

  (required) Choice between XYZ (default), LAB, or LCh colour models.

- visual:

  the visual system used when estimating XYZ values, if `vismodeldata`
  are not the result of a call to
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md)
  (otherwise the argument is ignored). Options are:

  - a data frame such as one produced containing by
    [`sensmodel()`](https://pavo.colrverse.com/reference/sensmodel.md),
    containing user-defined sensitivity data for the receptors involved
    in colour vision. The data frame must contain a `'wl'` column with
    the range of wavelengths included, and the sensitivity for each
    other cone as a column.

  - `'cie2'`: 2-degree colour matching functions for CIE models of human
    colour vision. Functions are linear transformations of the 2-degree
    cone fundamentals of Stockman & Sharpe (2000), as ratified by the
    CIE (2006).

  - `'cie10'`: 10-degree colour matching functions for CIE models of
    human colour vision. Functions are linear transformations of the
    10-degree cone fundamentals of Stockman & Sharpe (2000), as ratified
    by the CIE (2006).

- illum:

  the illuminant used when estimating XYZ values, if `vismodeldata` are
  not the result of a call to
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md)
  (otherwise the argument is ignored). Either a data frame containing a
  `'wl'` column and the illuminant spectrum, or one of the built-in
  options:

  - `'D65'`: standard daylight.

  - `'bluesky'` open blue sky.

  - `'forestshade'` forest shade.

## Value

Object of class
[`colspace`](https://pavo.colrverse.com/reference/colspace.md)
containing:

- `X, Y, Z`: Tristimulus values.

- `x, y, z`: Cartesian coordinates, when using `space = XYZ`.

- `L, a, b`: Lightness, `L`, and colour-opponent `a` (redness-greenness)
  and `b` (yellowness-blueness) values, in a Cartesian coordinate space.
  Returned when using `space = LAB`.

- `L, a, b, C, h`: Lightness, `L`, colour-opponent `a`
  (redness-greenness) and `b` (yellowness-blueness) values, as well as
  chroma `C` and hue-angle `h` (degrees), the latter of which are
  cylindrical representations of `a` and `b` from the CIELAB model.
  Returned when using `space = LCh`.

## References

Smith T, Guild J. (1932) The CIE colorimetric standards and their use.
Transactions of the Optical Society, 33(3), 73-134.

Westland S, Ripamonti C, Cheung V. (2012). Computational colour science
using MATLAB. John Wiley & Sons.

Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of the
middle- and long-wavelength sensitive cones derived from measurements in
observers of known genotype. Vision Research, 40, 1711-1737.

CIE (2006). Fundamental chromaticity diagram with physiological axes.
Parts 1 and 2. Technical Report 170-1. Vienna: Central Bureau of the
Commission Internationale de l Eclairage.

## Author

Thomas White <thomas.white026@gmail.com>

## Examples

``` r
# Load floral reflectance spectra
data(flowers)

# Estimate quantum catches, using the cie10-degree viewer matching function
vis.flowers <- vismodel(flowers, visual = "cie10", illum = "D65", vonkries = TRUE, relative = FALSE)

# Model floral spectra in the CIEXYZ space
flowers.ciexyz <- colspace(vis.flowers, space = "ciexyz")

# Model floral spectra in the CIELab space
flowers.cielab <- colspace(vis.flowers, space = "cielab")

# Model floral spectra in the CIELch space
flowers.cielch <- colspace(vis.flowers, space = "cielch")
```
