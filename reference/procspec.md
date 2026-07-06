# Process spectra

Applies normalization and/or smoothing to spectra for further analysis
or plotting.

## Usage

``` r
procspec(
  rspecdata,
  opt = c("none", "smooth", "maximum", "minimum", "bin", "sum", "center", "clip"),
  fixneg = c("none", "addmin", "zero"),
  span = 0.25,
  bins = 20,
  clip_range = NULL
)
```

## Arguments

- rspecdata:

  (required) a data frame, possibly of class `rspec`, which contains a
  column containing a wavelength range, named 'wl', and spectra data in
  remaining columns.

- opt:

  what type of processing options to apply. User can select multiple
  options by providing a vector. Possibilities are:

  - `"none"` does not perform any processing (default).

  - `"smooth"` applies LOESS smoothing to each spectrum using
    [`loess.smooth()`](https://rdrr.io/r/stats/scatter.smooth.html).
    Optimal smoothing parameter can be assessed by using
    [`plotsmooth()`](https://pavo.colrverse.com/reference/plotsmooth.md).

  - `"minimum"` subtracts the minimum from each individual spectra.

  - `"maximum"` divides each spectrum by its maximum value.

  - `"sum"` divides each spectrum by summed values.

  - `"bin"` bins each spectrum into the specified number of bins. `bins`
    argument must be set.

  - `"center"` centers individual spectra by subtracting mean
    reflectance from all values.

  - `"clip"` removes a specified range of wavelengths and replaces them
    by linear interpolation (clipping occurs before smoothing).
    `clip_range` must be provided.

- fixneg:

  how to handle negative values. Possibilities are:

  - `"none"` does not perform negative value correction (default).

  - `"zero"` sets all negative values to zero.

  - `"addmin"` adds the absolute value of the maximally negative values
    of each spectra to the reflectance at all other wavelengths (setting
    the minimum value to zero, but scaling other values accordingly).

- span:

  sets the smoothing parameter used by
  [`loess.smooth()`](https://rdrr.io/r/stats/scatter.smooth.html).

- bins:

  sets the number of equally sized wavelength bins for `opt = "bin"`.

- clip_range:

  either a numeric vector indicating the two bounds of the range of
  wavelengths to clip for `opt = "clip"`, or a list of such numeric
  vectors if multiple ranges are to be clipped.

## Value

A data frame of class `rspec` with the processed data.

## References

Cuthill, I., Bennett, A. T. D., Partridge, J. & Maier, E. 1999. Plumage
reflectance and the objective assessment of avian sexual dichromatism.
The American Naturalist, 153, 183-200.

Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J.,
eds. Bird Coloration. Volume 1 Mechanisms and measurements. Harvard
University Press, Cambridge, Massachusetts.

White, T. E., Dalrymple, R. L., Noble D. W. A., O'Hanlon, J. C., Zurek,
D. B., Umbers, K. D. L. 2015. Reproducible research in the study of
biological coloration. Animal Behaviour, 106, 51-57.

## See also

[`loess.smooth()`](https://rdrr.io/r/stats/scatter.smooth.html),
[`plotsmooth()`](https://pavo.colrverse.com/reference/plotsmooth.md)

## Author

Chad Eliason <cme16@zips.uakron.edu>

## Examples

``` r
data(teal)
plot(teal, select = 10)


# Smooth data to remove noise
teal.sm <- procspec(teal, opt = "smooth", span = 0.25)
#> processing options applied:
#> smoothing spectra with a span of 0.25
plot(teal.sm, select = 10)


# Normalize to max of unity
teal.max <- procspec(teal, opt = c("max"))
#> processing options applied:
#> Scaling spectra to a maximum value of 1
plot(teal.max, select = 10)


# Smoothing directly severe artifacts can artificially modify the shape of
# the entire spectrum. In this case, it is better to clip the artifact and
# then smooth the data.
teal_clip <- procspec(teal, opt = c("clip", "smooth"), clip_range = c(600, 650), span = 0.25)
#> processing options applied:
#> clipping spectra in the following wavelength ranges: 600-650
#> smoothing spectra with a span of 0.25
plot(teal_clip, select = 10)

```
