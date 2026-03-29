# Identify colour classes in an image for adjacency analyses

Classify image pixels into discrete colour classes.

## Usage

``` r
classify(
  imgdat,
  method = c("kMeans", "kMedoids"),
  kcols = NULL,
  refID = NULL,
  interactive = FALSE,
  plotnew = FALSE,
  col = "red",
  ...
)
```

## Arguments

- imgdat:

  (required) image data. Either a single image, or a series of images
  stored in a list. Preferably the result of
  [`getimg()`](https://pavo.colrverse.com/reference/getimg.md).

- method:

  methods for image segmentation/classification.

  - `'kMeans'`: k-means clustering (default)

  - `'kMedoids'`: k-medoids clustering, using the
    partitioning-around-medoids ('pam') algorithm for large datasets.

- kcols:

  the number of discrete colour classes present in the input image(s).
  Can be a single integer when only a single image is present, or if
  kcols is identical for all images. When passing a list of images,
  `kcols` can also be a vector the same length as `imgdat`, or a
  data.frame with two columns specifying image file names and
  corresponding kcols. This argument can optionally be disregarded when
  `interactive = TRUE`, and kcols will be inferred from the number of
  selections.

- refID:

  either the numeric index or name of a 'reference' image, for use when
  passing a list of images. Other images will be k-means classified
  using centres identified in the single reference image, thus helping
  to ensure that homologous pattern elements will be reliably classified
  between images, if so desired.

- interactive:

  interactively specify the colour-category 'centers', for k-means
  clustering. When `TRUE`, the user is asked to click a number of points
  (equal to `kcols`, if specified, otherwise user-determined) that
  represent the distinct colours of interest. If a reference image is
  specified, it will be the only image presented.

- plotnew:

  Should plots be opened in a new window when `interactive = TRUE`?
  Defaults to `FALSE`.

- col:

  the color of the marker points, when `interactive = TRUE`.

- ...:

  additional graphical parameters when `interactive = TRUE`. Also see
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html).

## Value

A matrix, or list of matrices, of class `rimg` containing the colour
class classifications ID at each pixel location. The RGB values
corresponding to cluster centres (i.e. colour classes) are stored as
object attributes.

## Details

You can customise the type of parallel processing used by this function
with the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
function. This works on all operating systems, as well as high
performance computing (HPC) environment. Similarly, you can customise
the way progress is shown with the
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
functions (progress bar, acoustic feedback, nothing, etc.)

## Note

Since the `kmeans` process draws on random numbers to find initial
cluster centres when `interactive = FALSE`, use
[`set.seed()`](https://rdrr.io/r/base/Random.html) if reproducible
cluster ID's are desired between runs.

## See also

[stats::kmeans](https://rdrr.io/r/stats/kmeans.html)

## Author

Thomas E. White <thomas.white026@gmail.com>

## Examples

``` r
# Single image
papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
papilio_class <- classify(papilio, kcols = 4)
#> Image classification in progress...

# Multiple images, with interactive classification and a reference image
snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#> 2 files found; importing images.
if (interactive()) {
  snakes_class <- classify(snakes, refID = "snake_01", interactive = TRUE)
}
```
