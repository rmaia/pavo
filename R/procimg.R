#' Process images
#'
#' Manually specify the scale of images.
#'
#' @param image (required) image data. Either a single image array, or a number of images
#' stored in a list. Preferably the result of \code{\link{getimg}}.
#' @param scaledist an integer specifying the length of the scale
#' in the image(s), if desired.
#' @param select_focal allows the user to interactively specify the focal object in
#' an image by clicking around its outline. The xy-coordinates of the resulting
#' polygon are saved as an attribute, for use in genrating a masking layer
#' in further analyses. This is particularly useful when backgrounds are complex,
#' (e.g. in natural settings) in which case backgrounds and objects cannot be
#' readily separated by simple k-means clustering.
#' @param smooth should the polygon specified when \code{select_focal = TRUE} be smoothed
#' using Chaikin's corner-cuting algorithm? Defaults to \code{FALSE}.
#' @param refinements the number of smoothing iterations, when \code{smooth = TRUE}.
#' @param plotnew Should plots be opened in a new window? Defaults to \code{FALSE}.
#'
#' @return an image array, or list containing images, for use in further
#' \code{pavo} functions, with scales stored as an attribute.
#'
#' @export
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio <- procimg(papilio, scale = 10)
#'
#' # Multiple images. Assign a scale an specify the nuber of colours present.
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes <- procimg(snakes, scale = 100)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @references Chaikin, G. 1974. An algorithm for high speed curve generation.
#' Computer Graphics and Image Processing 3, 346-349.

procimg <- function(image, scaledist = NULL, select_focal = FALSE, smooth = FALSE,
                    refinements = 1L, plotnew = FALSE) {

  ## Checks
  multi_image <- inherits(image, "list") # Single or multiple images?
  if (is.null(scaledist) & !select_focal) {
    stop("No options selected.")
  }

  if (multi_image) { # Multiple images

    ## Scale ##
    if (is.numeric(scaledist)) {
      if (plotnew) dev.new(noRStudioGD = TRUE)
      message("Scale calibration: Select both ends of the scale, images will progress automatically.")
      for (i in 1:length(image)) {
        attr(image[[i]], "scale") <- scaler(image_i = image[[i]], scaledist_i = scaledist)
      }
      if (plotnew) dev.off()
    }

    ## Select outline of focal stimulus ##
    if (select_focal) {
      if (plotnew) dev.new(noRStudioGD = TRUE)
      for (i in 1:length(image)) {
        message("Select the outline of focal stimulus, and press [esc] when complete.
                The first and last points will be automatically connected.")
        attr(image[[i]], "outline") <- outliner(image[[i]], smooth, refinements)
      }
      if (plotnew) dev.off()
    }
  } else if (!multi_image) {

    ## Scale ##
    if (plotnew) dev.new(noRStudioGD = TRUE)
    if (is.numeric(scaledist)) {
      message("Scale calibration: Select both ends of the scale.")
      attr(image, "px_scale") <- scaler(image_i = image, scaledist_i = scaledist)
    }
    if (plotnew) dev.off()

    ## Select outline of focal stimulus ##
    if (plotnew) dev.new(noRStudioGD = TRUE)
    if (select_focal) {
      message("Select the outline of focal stimulus, and press [esc] when complete.
              The first and last points will be automatically connected.")
      attr(image, "outline") <- outliner(image, smooth, refinements)
    }
    if (plotnew) dev.off()
  }

  image
}

#' Internal function for calibrating image scale
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
scaler <- function(image_i, scaledist_i) {
  plot(c(1, dim(image_i)[1]), c(1, dim(image_i)[2]), type = "n", xlab = "x", ylab = "y")
  rasterImage(image_i, 1, 1, dim(image_i)[1], dim(image_i)[2])

  reference <- as.data.frame(locator(type = "l", col = "red", n = 2))
  pixdist <- as.integer(dist(round(reference)))
  output <- scaledist_i / pixdist

  output
}

#' Internal function for selecting focal-stimulus outline
#'
#' @param image_i (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param smooth_i should the polygon specified when \code{select_focal = TRUE} be smoothed
#' using Chaikin's corner-cuting algorithm? Defaults to \code{FALSE}.
#' @param refinements_i the number of smoothing iterations, when \code{smooth = TRUE}.
#'
#' @keywords internal
#'
#' @return an image, or list containing images, for use in further
#' \code{pavo} functions, with scales stored as an attribute.
#'
outliner <- function(image_i, smooth_i, refinements_i) {

  # Plot
  plot(c(1, dim(image_i)[1]), c(1, dim(image_i)[2]), type = "n", xlab = "x", ylab = "y")
  rasterImage(image_i, 1, 1, dim(image_i)[1], dim(image_i)[2])

  # Get coordinates
  xy <- locator(type = "p", col = "red", lwd = 2)
  xy <- cbind(xy$x, xy$y)
  #xy <- rbind(xy, xy[1, ])

  # Smooth coordinates (Chaikin’s corner cutting)
  if (smooth_i) {
    for (i in seq.int(refinements_i)) {
      n_pts <- nrow(xy)
      qr <- matrix(NA_real_,
        nrow = 2 * (n_pts - 1) + 1,
        ncol = 2
      )
      qr[seq(1, nrow(qr) - 1, by = 2), ] <- 0.75 * xy[-n_pts, ] + 0.25 * xy[-1, ]
      qr[seq(2, nrow(qr) - 1, by = 2), ] <- 0.75 * xy[-1, ] + 0.25 * xy[-n_pts, ]
      qr[nrow(qr), ] <- qr[1, ]
      xy <- qr
    }
  }

  xy <- as.data.frame(xy)
  names(xy) <- c("x", "y")
  lines(xy)

  xy
}
