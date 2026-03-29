# Import spectra files

Finds and imports spectra files from a folder. Currently works for
reflectance files generated in Ocean Optics SpectraSuite (USB2000,
USB4000 and Jaz spectrometers), CRAIC software (after exporting) and
Avantes (before or after exporting).

## Usage

``` r
getspec(
  where = getwd(),
  ext = "txt",
  lim = c(300, 700),
  decimal = ".",
  sep = NULL,
  subdir = FALSE,
  subdir.names = FALSE,
  ignore.case = TRUE
)
```

## Arguments

- where:

  Folder in which files are located (defaults to current working
  directory).

- ext:

  File extension to be searched for, without the "." (defaults to
  `txt`). You can also use a character vector to specify multiple file
  extensions.

- lim:

  A vector with two numbers determining the wavelength limits to be
  considered (defaults to `c(300, 700)`).

- decimal:

  Character to be used to identify decimal plates (defaults to `.`).

- sep:

  Column delimiting characters to be considered in addition to the
  default (which are: tab, space, and ";")

- subdir:

  Should subdirectories within the `where` folder be included in the
  search? (defaults to `FALSE`).

- subdir.names:

  Should subdirectory path be included in the name of the spectra?
  (defaults to `FALSE`).

- ignore.case:

  Should the extension search be case insensitive? (defaults to `TRUE`)

## Value

A data frame, of class `rspec`, containing individual imported spectral
files as columns. Reflectance values are interpolated to the nearest
wavelength integer.

## Details

You can customise the type of parallel processing used by this function
with the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
function. This works on all operating systems, as well as high
performance computing (HPC) environment. Similarly, you can customise
the way progress is shown with the
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
functions (progress bar, acoustic feedback, nothing, etc.)

## References

Gruson H, White TE, Maia R (2019) lightr: import spectral data and
metadata in R. Journal of Open Source Software, 4(43), 1857,
[doi:10.21105/joss.01857](https://doi.org/10.21105/joss.01857) .

## See also

[`lightr::lr_get_spec()`](https://docs.ropensci.org/lightr/reference/lr_get_spec.html)
for a more flexible version of this function (e.g. uninterpolated
wavelengths), and
[`lightr::lr_get_metadata()`](https://docs.ropensci.org/lightr/reference/lr_get_metadata.html)
for the retrieval and import of spectral metadata. See
<https://docs.ropensci.org/lightr/> for the complete, and up-to-date,
list of supported file formats.

## Author

Rafael Maia <rm72@zips.uakron.edu>

Hugo Gruson <hugo.gruson+R@normalesup.org>

## Examples

``` r
# Import and inspect example spectral data with a range of set to 400-700nm.
rspecdata <- getspec(system.file("testdata", package = "lightr"), ext = "ttt", lim = c(400, 700))
#> 2 files found; importing spectra:
head(rspecdata)
#>    wl avantes_export avantes_export_long
#> 1 400         4.0899            5.639232
#> 2 401         3.9514            6.342615
#> 3 402         4.1269            6.147300
#> 4 403         4.2617            6.010692
#> 5 404         4.1683            6.333537
#> 6 405         4.0871            6.490569
```
