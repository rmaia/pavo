# Convert data to an rspec object

Converts data frames or matrices containing spectral data to `rspec`
object

## Usage

``` r
as.rspec(
  object,
  whichwl = NULL,
  interp = TRUE,
  lim = NULL,
  exceed.range = TRUE
)

is.rspec(object)
```

## Arguments

- object:

  (required) a data frame or matrix containing spectra to process.

- whichwl:

  a numeric or character vector specifying which column contains
  wavelengths. If `NULL` (default), function searches for column
  containing equally spaced numbers and sets it as wavelengths "wl". If
  no wavelengths are found or `whichwl` is not given, returns arbitrary
  index values.

- interp:

  whether to interpolate wavelengths in 1-nm bins (defaults to `TRUE`).
  It is rarely recommended to turn off this option, as uninterpolated
  spectra are incompatible with some downstream analyses, including
  notably colour vision models.

- lim:

  vector specifying wavelength range to interpolate over (e.g.
  `c(300, 700)`).

- exceed.range:

  logical. Should data be interpolated to the limits specified by `lim`
  if `lim` exceeds the range of the actual data? Useful, and relatively
  safe, when the data range falls slightly within `lim` (e.g. 300.1 -
  699 nm), but will produce spurious results if `lim` far exceeds the
  range of input data. Defaults to `TRUE`.

## Value

an object of class `rspec` for use in further `pavo` functions

a logical value indicating whether the object is of class `rspec`

## Author

Chad Eliason <cme16@zips.uakron.edu>

## Examples

``` r
# Generate some fake reflectance data
fakedat <- data.frame(wl = 300:700, refl1 = rnorm(401), refl2 = rnorm(401))
head(fakedat)
#>    wl      refl1      refl2
#> 1 300  1.1947611 -0.1074917
#> 2 301 -1.7410191 -0.5953402
#> 3 302 -0.4499410  0.4799968
#> 4 303 -0.3191096 -0.6647274
#> 5 304 -1.3198173 -1.2601080
#> 6 305 -2.2864853 -1.4044018

# Determine if is rspec object
is.rspec(fakedat)
#> [1] FALSE

# Convert to rspec object
fakedat2 <- as.rspec(fakedat)
#> wavelengths found in column 1
#> The spectral data contain 428 negative value(s),
#>  which may produce unexpected results if used in models.
#> Consider using procspec() to correct them.
is.rspec(fakedat2)
#> [1] TRUE
head(fakedat2)
#>    wl      refl1      refl2
#> 1 300  1.1947611 -0.1074917
#> 2 301 -1.7410191 -0.5953402
#> 3 302 -0.4499410  0.4799968
#> 4 303 -0.3191096 -0.6647274
#> 5 304 -1.3198173 -1.2601080
#> 6 305 -2.2864853 -1.4044018
```
