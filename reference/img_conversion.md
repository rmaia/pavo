# Convert images between class rimg and cimg or magick-image

Conveniently convert single objects of class `rimg` to class `cimg`
(from the package `imager`) or `magick-image` (from the package
`magick`), both of which contains a suite of useful image-processing
capabilities.

## Usage

``` r
# S3 method for class 'rimg'
as.cimg(image)

rimg2magick(image)
```

## Arguments

- image:

  an object of class `rimg`

## Value

an image of the specified class

## Note

Attributes (e.g. scales, color-classes) will not be preserved following
conversion from class `rimg`, so it's best to use early in the analysis
workflow.

## Author

Thomas E. White <thomas.white026@gmail.com>

Hugo Gruson <hugo.gruson+R@normalesup.org>

## Examples

``` r
papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
# \donttest{
# Convert from class rimg to cimg
if (requireNamespace("imager", quiety = TRUE)) {
  papilio_cimg <- rimg2cimg(papilio)
  class(papilio_cimg)
}
#> Loading required namespace: imager
# }

# Convert from class rimg to magick-image
papilio_magick <- rimg2magick(papilio)
class(papilio_magick)
#> [1] "magick-image"
```
