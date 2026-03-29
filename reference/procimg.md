# Process images

Specify scales, resize, and/or define focal objects within images.

## Usage

``` r
procimg(
  image,
  resize = NULL,
  rotate = NULL,
  scaledist = NULL,
  outline = FALSE,
  reclass = NULL,
  smooth = FALSE,
  iterations = 1L,
  col = "red",
  obj_dist = NULL,
  obj_width = NULL,
  eye_res = NULL,
  plotnew = FALSE,
  ...
)
```

## Arguments

- image:

  (required) image data. Either a single image array, or a number of
  images stored in a list. Preferably the result of
  [`getimg()`](https://pavo.colrverse.com/reference/getimg.md).

- resize:

  an integer specifying a percentage for resizing images, if so desired.
  E.g. 50 to half the size of an image, 200 to double it.

- rotate:

  an integer specifying the angle of image rotation, in degrees. Images
  are rotated around the centre, and linearly interpolated.

- scaledist:

  an integer, or numeric vector equal in length to the number of images,
  specifying the length of the scale in the image(s). Image(s) will then
  be presented, and the user asked to select either end of the scale
  corresponding to the input value.

- outline:

  interactively specify the focal object in an image by clicking around
  its outline. The xy-coordinates of the resulting closed polygon are
  saved as an attribute, for use in generating a masking layer &
  separating animals/plants from backgrounds in further analyses. This
  is particularly useful when backgrounds are complex, such as in
  natural settings.

- reclass:

  interactively specify an area on a colour-classified image that is to
  be reclassified as the numeric value provided. e.g. when
  `reclass = 1`, the user will be asked to select a polygon on the
  image, within which all colour-category values will be changes to `1`.

- smooth:

  should the polygon specified when `outline = TRUE` be smoothed using
  Chaikin's corner-cuting algorithm? Defaults to `FALSE`.

- iterations:

  the number of smoothing iterations, when `smooth = TRUE`. Defaults to
  `1`.

- col:

  the color of the marker points and/or line, when using interactive
  options.

- obj_dist, obj_width, eye_res:

  blur the image to model the visual acuity of non-human animals as per
  Caves & Johnsen (2018)'s AcuityView 2.0 algorithm. The procedure
  requires three arguments; obj_dist is the real-world distance between
  the viewer and the focal object in the image in the image, obj_width
  is the real-world width of the entire image; eye_res is the minimum
  resolvable angle of the viewer in degrees. All three arguments are
  numeric, and any units of measurement are suitable for obj_dist and
  obj_width, but they must match. Note that this is the more flexible
  v2.0 implementation meaning that any rectangular image is suitable; it
  need not be square with dimensions a power of 2. If using this
  capability, please cite Caves & Johnsen (2018), as per the included
  reference, and see note below.

- plotnew:

  should plots be opened in a new window? Defaults to `FALSE`.

- ...:

  additional graphical parameters. Also see
  [`par()`](https://rdrr.io/r/graphics/par.html).

## Value

an image, or list of images, for use in further `pavo` functions.

## Note

There are several caveats that should be considered when using the
AcuityView algorithm. First and foremost, the converted image is not
what the animal actually sees. For example, it does not account for edge
enhancement and other processing by the retina and brain that may alter
an image. It does, however, show what spatial information can be
detected and then processed by the visual system. Second, the converted
image is static, which does not allow one to assess how movement may
reveal the presence of an otherwise indiscernible object. Third,
AcuityView makes several assumptions about the Modulation Transfer
Function (MTF), which describes how the optical system affects image
contrast as a function of the level of detail. These assumptions include
that the MTF is constant over the region of the retina that views the
scene, is circularly symmetrical, and is wavelength independent. For a
full discussion and details, please do read Caves & Johnsen (2018).

## References

Caves, E. M., & Johnsen, S. (2018). AcuityView: An r package for
portraying the effects of visual acuity on scenes observed by an animal.
Methods in Ecology and Evolution, 9(3), 793-797
[doi:10.1111/2041-210X.12911](https://doi.org/10.1111/2041-210X.12911) .

Chaikin, G. 1974. An algorithm for high speed curve generation. Computer
Graphics and Image Processing 3, 346-349.

## Author

Thomas E. White <thomas.white026@gmail.com>

## Examples

``` r
if (interactive()) {
  # Interactively add a scale to a single image
  papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
  papilio <- procimg(papilio, scaledist = 10)

  # Interactively assign individual scales to each image,
  # after slightly reducing their size (to 90% of original).
  snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
  snakes <- procimg(snakes, scaledist = c(10, 14), resize = 90)

  # Model the appearance of a butterfly given the reduced visual acuity of another
  # animal viewer as per the AcuityView algorithm. Here our butterfly is 60 cm away,
  # the image width is 10 cm, and the minimum resolvable angle of the viewer is 0.2-degrees.
  tiger <- getimg(system.file("testdata/images/tiger.png", package = "pavo"))
  tiger_acuity <- procimg(tiger, obj_dist = 60, obj_width = 10, eye_res = 0.2)
}
```
