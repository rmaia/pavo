# Run an adjacency and boundary strength analysis

Calculate summary variables from the adjacency (Endler 2012) and
boundary-strength (Endler et al. 2018) analyses, along with overall
pattern contrast (Endler & Mielke 2005).

## Usage

``` r
adjacent(
  classimg,
  xpts = NULL,
  xscale = NULL,
  bkgID = NULL,
  polygon = NULL,
  exclude = c("none", "background", "object"),
  coldists = NULL,
  hsl = NULL
)
```

## Arguments

- classimg:

  (required) an xyz matrix, or list of matrices, in which x and y
  correspond to spatial (e.g. pixel) coordinates, and z is a numeric
  code specifying a colour-class. Preferably the result of
  [`classify()`](https://pavo.colrverse.com/reference/classify.md), or
  constructed from grid-sampled spectra that have been visually modelled
  and clustered (as per Endler 2012).

- xpts:

  an integer specifying the number of sample points along the x axis,
  from which the evenly-spaced sampling grid is constructed (if
  required). Defaults to the smallest dimension of `classimg`, though
  this should be carefully considered.

- xscale:

  (required) an integer or list of integers equal in length to
  classimg() specifying the true length of the x-axis, in preferred
  units. Not required, and ignored, only if image scales have been set
  via [`procimg()`](https://pavo.colrverse.com/reference/procimg.md).

- bkgID:

  an integer or vector specifying the colour-class ID number(s) of
  pertaining to the background alone, for relatively homogeneous and
  uniquely-identified backgrounds (e.g. the matte background of pinned
  specimens). Examine the attributes of, or call `summary` on, the
  result of
  [`classify()`](https://pavo.colrverse.com/reference/classify.md) to
  visualise the RGB values corresponding to colour-class ID numbers for
  classified images. Ignored if the focal object and background has been
  identified using
  [`procimg()`](https://pavo.colrverse.com/reference/procimg.md).

- polygon:

  a data.frame of x-y coordinates delineating a closed polygon that
  separates the focal object from the background. Not required, and
  ignored, if the focal object outline is specified using
  [`procimg()`](https://pavo.colrverse.com/reference/procimg.md).

- exclude:

  the portion of the scene to be excluded from the analysis, if any.

  - `'none'`: default

  - `'background'`: exclude everything *outside* the closed polygon
    specified using
    [`procimg()`](https://pavo.colrverse.com/reference/procimg.md), or
    the argument `polygon`. Alternatively, if the background is
    relatively homogeneous the colour-class ID(s) uniquely corresponding
    to the background can be specified via `bkgID`, and subsequently
    excluded.

  - `'object'`: exclude everything *inside* the closed polygon specified
    using
    [`procimg()`](https://pavo.colrverse.com/reference/procimg.md), or
    the argument `polygon`.

- coldists:

  a data.frame specifying the visually-modelled chromatic (dS) and/or
  achromatic (dL) distances between colour-categories. The first two
  columns should be named 'c1' and 'c2', and specify all possible
  combinations of numeric colour-class ID's (viewable by calling
  `summary(image, plot = TRUE)` on a colour classified image), with the
  remaining columns named dS (for chromatic distances) and/or dL (for
  achromatic distances). See
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) and
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md) for
  visual modelling with spectral data.

- hsl:

  data.frame specifying the hue, saturation, and luminance of color
  patch elements, as might be estimated via
  [`vismodel()`](https://pavo.colrverse.com/reference/vismodel.md) and
  [`colspace()`](https://pavo.colrverse.com/reference/colspace.md). The
  first column, named 'patch', should contain numeric color category
  IDs, with the remaining columns specifying one or more of 'hue'
  (angle, in radians), 'sat', and/or 'lum'.

## Value

a data frame of summary variables:

- `'k'`: The number of user-specified colour and/or luminance classes.

- `'N'`: The grand total (sum of diagonal and off-diagonal) transitions.

- `'n_off'`: The total off-diagonal transitions.

- `'p_i'`: The overall frequency of colour class *i*.

- `'q_i_j'`: The frequency of transitions between *all* colour classes
  *i* and *j*, such that `sum(q_i_j) = 1`.

- `'t_i_j'`: The frequency of off-diagonal (i.e. class-change
  transitions) transitions *i* and *j*, such that `sum(t_i_j) = 1`.

- `'m'`: The overall transition density (mean transitions), in units
  specified in the argument `xscale`.

- `'m_r'`: The row-wise transition density (mean row transitions), in
  user-specified units.

- `'m_c'`: The column-wise transition density (mean column transitions),
  in user-specified units.

- `'A'`: The transition aspect ratio (\< 1 = wide, \> 1 = tall).

- `'Sc'`: Simpson colour class diversity, `Sc = 1/(sum(p_i^2))`. If all
  colour and luminance classes are equal in relative area, then
  `Sc = k`.

- `'St'`: Simpson transition diversity, `St = 1/sum(t_i_j^2)`.

- `'Jc'`: Simpson colour class diversity relative to its achievable
  maximum. `Jc = Sc/k`.

- `'Jt'`: Simpson transition diversity relative to its achievable
  maximum. `Jt = St/(k*(k-1)/2)`.

- `'B'`: The animal/background transition ratio, or the ratio of
  class-change transitions entirely within the focal object and those
  involving the object and background, `B = sum(O_a_a / O_a_b)`.

- `'Rt'`: Ratio of animal-animal and animal-background transition
  diversities, `Rt = St_a_a / St_a_b`.

- `'Rab'`: Ratio of animal-animal and background-background transition
  diversities, `Rt = St_a_a / St_b_b`.

- `'m_dS', 's_dS', 'cv_dS'`: weighted mean, sd, and coefficient of
  variation of the chromatic boundary strength.

- `'m_dL', 's_dL', 'cv_dL'`: weighted mean, sd, and coefficient of
  variation of the achromatic boundary strength.

- `'m_hue', 's_hue', 'var_hue'`: circular mean, sd, and variance of
  overall pattern hue (in radians).

- `'m_sat', 's_sat', 'cv_sat'`: weighted mean, sd, and coefficient
  variation of overall pattern saturation.

- `'m_lum', 's_lum', 'cv_lum'`: weighted mean, sd, and coefficient
  variation of overall pattern luminance.

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

Endler, J. A. (2012). A framework for analysing colour pattern geometry:
adjacent colours. Biological Journal Of The Linnean Society, 107(2),
233-253.

Endler, J. A., Cole G., Kranz A. (2018). Boundary Strength Analysis:
Combining color pattern geometry and coloured patch visual properties
for use in predicting behaviour and fitness. Methods in Ecology and
Evolution, 9(12), 2334-2348.

Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as
birds see them. Biological Journal Of The Linnean Society, 86(4),
405-431.

## See also

[`classify()`](https://pavo.colrverse.com/reference/classify.md),
[`summary.rimg()`](https://pavo.colrverse.com/reference/summary.rimg.md),
[`procimg()`](https://pavo.colrverse.com/reference/procimg.md)

## Author

Thomas E. White <thomas.white026@gmail.com>

## Examples

``` r
# \donttest{
# Set a seed, for reproducibility
set.seed(153)

# Run the adjacency analysis on a single image of a butterfly
papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
papilio_class <- classify(papilio, kcols = 4)
#> Image classification in progress...
papilio_adj <- adjacent(papilio_class, xscale = 100)

# Expand on the above, by including (fake) color distances and hsl values
# of colour elements in the image

# Generate fake color distances
distances <- data.frame(
  c1 = c(1, 1, 1, 2, 2, 3),
  c2 = c(2, 3, 4, 3, 4, 4),
  dS = c(5.3, 3.5, 5.7, 2.9, 6.1, 3.2),
  dL = c(5.5, 6.6, 3.3, 2.2, 4.4, 6.6)
)

# Generate some fake hue, saturation, luminance values
hsl_vals <- data.frame(
  patch = seq_len(4),
  hue = c(1.5, 2.2, 1.0, 0.5),
  lum = c(10, 5, 7, 3),
  sat = c(3.5, 1.1, 6.3, 1.3)
)

# Run the full analysis, including the white background's ID
papilio_adj <- adjacent(papilio_class,
  xscale = 100, bkgID = 1,
  coldists = distances, hsl = hsl_vals
)
#> Using single set of coldists for all images.
#> Using single set of hsl values for all images.

# Run an adjacency analysis on multiple images.
# First load some images of coral snake colour patterns
snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#> 2 files found; importing images.

# Automatically colour-classify the coral snake patterns
snakes_class <- classify(snakes, kcols = 3)
#> Image classification in progress...

# Run the adjacency analysis, with varying real-world scales for each image
snakes_adj <- adjacent(snakes_class, xpts = 120, xscale = c(50, 55))
# }
```
