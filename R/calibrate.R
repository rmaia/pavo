#' Calibrate images
#'
#' Manually specify the scale of images.
#'
#' @param image (required) image data. Either a single image array, or a number of images
#' stored in a list. Preferably the result of \code{\link{getimg}}.
#' @param scale_length an integer specifying the length of the scale
#' in the image, if desired.
#' @param assign_n do you want to interactively specify the number of discrete colour
#' classes present in each image (i.e. the k for k-means clustering)? Useful
#' when the number of colour categories varies between images. Defaults to \code{FALSE}.
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
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes <- calibrate(snakes, scale = 100, assign_n = TRUE)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'

calibrate <- function(image, scale_length = NULL, assign_n = FALSE) {

  ## Checks
  multi_image <- inherits(image, "list") # Single or multiple images?
  if(is.null(scale_length) && assign_n == FALSE)
    stop("No options selected.")

  if (isTRUE(multi_image)) { # Multiple images
    ## Assign k ##
    if (isTRUE(assign_n)) {
      message("n_col specification: Enter n_cols present in each image and press return, images will progress automatically.")
      for (i in 1:length(image)) {
        attr(image[[i]], "k") <- calibrate_main(image_i = image[[i]], scale_length_i = scale_length, type = "k")
      }
    }
    ## Scale ##
    if (is.numeric(scale_length)) {
      message("Scale calibration: Select both ends of the scale, images will progress automatically.")
      for (i in 1:length(image)) {
        attr(image[[i]], "scale") <- calibrate_main(image_i = image[[i]], scale_length_i = scale_length, type = "scale")
      }
    }
  } else if (!isTRUE((multi_image))) {
    ## Assign k ##
    if (isTRUE(assign_n)) {
      message("n_col specification: Enter n_cols present and press return.")
      attr(image, "k") <- calibrate_main(image_i = image, scale_length_i = scale_length, type = "k")
    }
    ## Scale ##
    if (is.numeric(scale_length)) {
      message("Scale calibration: Select both ends of the scale.")
      attr(image, "px_scale") <- calibrate_main(image_i = image, scale_length_i = scale_length, type = "scale")
    }
  }

  image
}

#' Main function for calibrating images
#'
#' @param image_i (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param scale_length_i (required) an integer specifying the length of the scale
#' in the image.
#' @param type internal consistency.
#'
#' @keywords internal
#'
#' @importFrom graphics plot rasterImage locator
#' @importFrom stats dist
#'
#' @return an image, or list containing images, for use in further
#' \code{pavo} functions, with scales stored as an attribute.
#'
calibrate_main <- function(image_i, scale_length_i, type = c("scale", "k")) {
  plot(c(1, dim(image_i)[1]), c(1, dim(image_i)[2]), type = "n", xlab = "x", ylab = "y")
  rasterImage(image_i, 1, 1, dim(image_i)[1], dim(image_i)[2])

  if (type == "scale") {
    reference <- as.data.frame(locator(type = "l", col = "red", n = 2))
    pixdist <- as.integer(dist(round(reference)))
    output <- scale_length_i / pixdist
  }
  if (type == "k") {
    output <- readline(prompt = paste0("Enter n_cols for ", attr(image_i, 'imgname'), ":"))
  }

  output
}
