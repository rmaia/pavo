#' Calibrate images
#'
#' Manually specify the scale of images.
#'
#' @param image (required) image data. Either a single image, or a series of images
#' stored in a list. Preferably the result of \code{\link{getimg}}.
#' @param scale_length (required) an integer specifying the length of the scale
#' in the image.
#'
#' @return an image, or list containing images, for use in further
#' \code{ptrn} functions, with scales stored as an attribute.
#'
#' @export
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'ptrn'))
#' papilio <- calibrate(papilio, scale = 10)
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'ptrn'))
#' snakes <- calibrate(snakes, scale = 100)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'

calibrate <- function(image, scale_length = NULL) {
  multi_image <- inherits(image, "list") # Single or multiple images?

  if (isTRUE(multi_image)) { # Multiple images
    cat("Select both ends of the scale. Images will progress automatically.")
    for (i in 1:length(image)) {
      attr(image[[i]], "scale") <- calibrate_main(image_i = image[[i]], scale_length_i = scale_length)
    }
  } else if (!isTRUE((multi_image))) {
    cat("Select both ends of the scale.")
    attr(image, "px_scale") <- calibrate_main(image_i = image, scale_length_i = scale_length)
  }

  image
}

#' Main function for calibrating images
#'
#' @param image_i (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param scale_length_i (required) an integer specifying the length of the scale
#' in the image.
#'
#' @keywords internal
#'
#' @return an image, or list containing images, for use in further
#' \code{ptrn} functions, with scales stored as an attribute.
#'
calibrate_main <- function(image_i, scale_length_i = NULL) {

  graphics::plot(c(1, dim(image_i)[1]), c(1, dim(image_i)[2]), type = "n", xlab = "x", ylab = "y")
  graphics::rasterImage(image_i, 1, 1, dim(image_i)[1], dim(image_i)[2])

  reference <- as.data.frame(graphics::locator(type = "l", col = "red", n = 2))
  pixdist <- as.integer(stats::dist(round(reference)))
  realscale <- scale_length_i / pixdist

  realscale
}
