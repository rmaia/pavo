# Import image data

Finds and imports PNG, JPEG, and/or BMP images.

## Usage

``` r
getimg(imgpath = getwd(), subdir = FALSE, subdir.names = FALSE, max.size = 1)
```

## Arguments

- imgpath:

  (required) either the full file-path or URL to an image (including
  extension), or the path to a folder in which multiple image files are
  located. Mixed file formats within a folder are accepted.

- subdir:

  should subdirectories within the `imgpath` folder be included in the
  search? (defaults to `FALSE`).

- subdir.names:

  should subdirectory path be included in the name of the images?
  (defaults to `FALSE`).

- max.size:

  maximum size of all images to be allowed in memory, in GB. Defaults to
  `1`.

## Value

a image, or list of images, of class `rimg`, for use in further `pavo`
functions.

## Author

Thomas E. White <thomas.white026@gmail.com>

## Examples

``` r
# Single image
papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))

# Multiple images
snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#> 2 files found; importing images.
```
