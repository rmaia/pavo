#' Calibrate images
#'
#' Manually specify the scale of images.
#'
#' @param image (required) image data. Either a single image array, or a number of images
#' stored in a list. Preferably the result of \code{\link{getimg}}.
#' @param scaledist an integer specifying the length of the scale
#' in the image(s), if desired.
#'
#' @return an image array, or list containing images, for use in further
#' \code{pavo} functions, with scales stored as an attribute.
#'
#' @export
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio <- calibrate(papilio, scale = 10)
#'
#' # Multiple images. Assign a scale an specify the nuber of colours present.
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes <- calibrate(snakes, scale = 100)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'

calibrate <- function(image, scaledist = NULL) {

  ## Checks
  multi_image <- inherits(image, "list") # Single or multiple images?
  if (is.null(scaledist)) {
    stop("No options selected.")
  }

  if (multi_image) { # Multiple images

    ## Scale ##
    if (is.numeric(scaledist)) {
      message("Scale calibration: Select both ends of the scale, images will progress automatically.")
      for (i in 1:length(image)) {
        attr(image[[i]], "scale") <- calibrate_main(image_i = image[[i]], scaledist_i = scaledist)
      }
    }
  } else if (!multi_image) {

    ## Scale ##
    if (is.numeric(scaledist)) {
      message("Scale calibration: Select both ends of the scale.")
      attr(image, "px_scale") <- calibrate_main(image_i = image, scaledist_i = scaledist)
    }
  }

  image
}

#' Main function for calibrating images
#'
#' @param image_i (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param scaledist_i (required) an integer specifying the length of the scale
#' in the image.
#'
#' @keywords internal
#'
#' @importFrom graphics plot rasterImage locator
#' @importFrom stats dist
#'
#' @return an image, or list containing images, for use in further
#' \code{pavo} functions, with scales stored as an attribute.
#'
calibrate_main <- function(image_i, scaledist_i) {
  
  plot(c(1, dim(image_i)[1]), c(1, dim(image_i)[2]), type = "n", xlab = "x", ylab = "y")
  rasterImage(image_i, 1, 1, dim(image_i)[1], dim(image_i)[2])
  reference <- as.data.frame(locator(type = "l", col = "red", n = 2))
  pixdist <- as.integer(dist(round(reference)))
  output <- scaledist_i / pixdist

  output
}
