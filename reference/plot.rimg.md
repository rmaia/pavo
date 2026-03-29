# Plot unprocessed or colour-classified images

Plot unprocessed or colour-classified image data. If the images are in a
list, they will be stepped through one by one.

## Usage

``` r
# S3 method for class 'rimg'
plot(x, axes = TRUE, col = NULL, ...)
```

## Arguments

- x:

  (required) an image of class rimg, or list thereof.

- axes:

  should axes be drawn? (defaults to `TRUE`)

- col:

  optional vector of colours when plotting colour-classified images.
  Defaults to the mean RGB values of the k-means centres (i.e. the
  average 'original' colours).

- ...:

  additional graphical parameters. Also see
  [`par()`](https://rdrr.io/r/graphics/par.html).

## Value

an image plot.

## Author

Thomas E. White <thomas.white026@gmail.com>

## Examples

``` r
papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
plot(papilio)

# \donttest{
papilio_class <- classify(papilio, kcols = 4)
#> Image classification in progress...
plot(papilio_class)

# }

# Multiple images
snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#> 2 files found; importing images.
plot(snakes)
#> Press [enter] for next plot

#> Press [enter] for next plot

# \donttest{
snakes_class <- classify(snakes, kcols = 3)
#> Image classification in progress...
plot(snakes_class)
#> Press [enter] for next plot

#> Press [enter] for next plot

# }
```
