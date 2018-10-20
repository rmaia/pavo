#' Process images
#'
#' Specify scales, resize, and/or define focal objects within images.
#'
#' @param image (required) image data. Either a single image array, or a number of images
#' stored in a list. Preferably the result of \code{\link{getimg}}.
#' @param resize an integer specifying the scaling factor for linearly
#' resizing images, if so desired. E.g. 0.5 to half the size of an image, or 2 to
#' double it.
#' @param rotate an integer specifying the angle of image rotation, in degrees. Images
#' are rotated around the centre, and linearly interpolated.
#' @param scaledist an integer, or numeric vector equal in length to the number of images,
#' specifying the length of the scale in the image(s). Image(s) will then be presented,
#' and the user asked to select either end of the scale corresponding to the input value.
#' @param outline interactively specify the focal object in
#' an image by clicking around its outline. The xy-coordinates of the resulting
#' closed polygon are saved as an attribute, for use in generating a masking layer &
#' separating animals/plants from backgrounds in further analyses. This is particularly
#' useful when backgrounds are complex, such as in natural settings.
#' @param col the color of the marker points and/or line, when using interactive options.
#' @param smooth should the polygon specified when \code{outline = TRUE} be smoothed
#' using Chaikin's corner-cuting algorithm? Defaults to \code{FALSE}.
#' @param iterations the number of smoothing iterations, when \code{smooth = TRUE}.
#' Defaults to \code{1}.
#' @param plotnew should plots be opened in a new window? Defaults to \code{FALSE}.
#' @param ... additional graphical parameters. Also see \code{\link{par}}.
#'
#' @return an image, or list of images, for use in further
#' \code{pavo} functions.
#'
#' @export
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio <- procimg(papilio, scaledist = 10)
#'
#' # Assign individual scales to each image, after slightly reducing their size.
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes <- procimg(snakes, scaledist = c(10, 14), resize = 0.95)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @references Chaikin, G. 1974. An algorithm for high speed curve generation.
#' Computer Graphics and Image Processing 3, 346-349.

procimg <- function(image, resize = NULL, rotate = NULL, scaledist = NULL,
                    outline = FALSE, smooth = FALSE, iterations = 1L, col = "red",
                    plotnew = FALSE, ...) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Class
  if (!"rimg" %in% class(image)) {
    message("Attempting to coerce image to class rimg.")
    if ("cimg" %in% class(image)) {
      image <- cimg2rimg(image)
    } else {
      image <- as.rimg(image)
    }
  }

  ## Options
  if (is.null(scaledist) && !outline && is.null(resize) && is.null(rotate)) {
    stop("No options selected.")
  }

  multi_image <- inherits(image, "list") # Single or multiple images?

  ## ------------------------------ Main ------------------------------ ##

  if (multi_image) { # Multiple images

    ## Resize ##
    if (attr(image[[1]], "state") == "colclass" && is.numeric(resize)) {
      message("Cannot resize colour-classified images.")
      resize <- NULL
    }
    if (is.numeric(resize)) {
      imgnames <- lapply(image, function(x) attr(x, "imgname"))
      image <- rimg2magick(image)
      image <- magick::image_scale(image, paste0(resize * 100, "%"))
      image <- magick2rimg(image, name = imgnames)
      class(image) <- c("rimg", "list")
    }

    ## Rotate ##
    if (attr(image[[1]], "state") == "colclass" && is.numeric(rotate)) {
      message("Cannot rotate colour-classified images.")
      rotate <- NULL
    }
    if (is.numeric(rotate)) {
      imgnames <- lapply(image, function(x) attr(x, "imgname"))
      image <- img2magick(image)
      image <- magick::image_rotate(image, rotate)
      image <- magick2rimg(image)
      class(image) <- c("rimg", "list")
    }

    ## Scale ##
    if (is.numeric(scaledist)) {

      # Formatting
      if (length(scaledist) == 1) {
        scaledist <- as.list(rep(scaledist, length(image)))
      }
      if (length(scaledist) > 1 && length(scaledist) != length(image)) {
        stop("Number of scales provided greater than one, but unequal to the the number of images. Provide a single scale to be recycled, or one per image.")
      }

      if (plotnew) dev.new(noRStudioGD = TRUE)
      message("Scale calibration: Select both ends of the scale, images will progress automatically.")
      for (i in 1:length(image)) {
        attr(image[[i]], "px_scale") <- scaler(image_i = image[[i]], scaledist_i = scaledist[[i]], col = col, ...)
        attr(image[[i]], "raw_scale") <- scaledist[[i]]
      }
      if (plotnew) dev.off()
    }

    ## Select outline ##
    if (outline) {
      if (plotnew) dev.new(noRStudioGD = TRUE)
      for (i in 1:length(image)) {
        message("Select the outline of focal stimulus, and press [esc] when complete.
                The first and last points will be automatically connected.")
        attr(image[[i]], "outline") <- outliner(image[[i]], smooth, iterations, col = col, ...)
      }
      if (plotnew) dev.off()
    }
  } else {

    ## Resize ##
    if (attr(image, "state") == "colclass" && is.numeric(resize)) {
      message("Cannot resize colour-classified images.")
      resize <- NULL
    }
    if (is.numeric(resize)) {
      imgname <- attr(image, "imgname")
      image <- rimg2magick(image)
      image <- magick::image_scale(image, paste0(resize * 100, "%"))
      image <- magick2rimg(image, imgname)
    }

    ## Rotate ##
    if (attr(image, "state") == "colclass" && is.numeric(rotate)) {
      message("Cannot rotate colour-classified images.")
      rotate <- NULL
    }
    if (is.numeric(rotate)) {
      imgname <- attr(image, "imgname")
      image <- rimg2magick(image)
      image <- magick::rotate(image, rotate)
      image <- magick2rimg(image, imgname)
    }

    ## Scale ##
    if (plotnew) dev.new(noRStudioGD = TRUE)
    if (is.numeric(scaledist)) {
      message("Scale calibration: Select both ends of the scale.")
      attr(image, "px_scale") <- scaler(image_i = image, scaledist_i = scaledist, col = col, ...)
      attr(image, "raw_scale") <- scaledist
    }
    if (plotnew) dev.off()

    ## Select outline of focal stimulus ##
    if (plotnew) dev.new(noRStudioGD = TRUE)
    if (outline) {
      message("Select the outline of focal stimulus, and press [esc] when complete.
              The first and last points will be automatically connected.")
      attr(image, "outline") <- outliner(image, smooth, iterations, col = col, ...)
    }
    if (plotnew) dev.off()
  }

  image
}

# Internal function for calibrating image scale
#' @importFrom graphics plot rasterImage locator
#' @importFrom stats dist
scaler <- function(image_i, scaledist_i, col, ...) {

  # Plot
  if (attr(image_i, "state") == "raw") {
    plot(image_i, axes = TRUE, col = NULL, ...)
  } else if (attr(image_i, "state") == "colclass") {
    plot(image_i, axes = TRUE, col = NULL, ...)
  }

  reference <- as.data.frame(locator(type = "l", col = col, n = 2))
  pixdist <- as.integer(dist(round(reference)))
  output <- scaledist_i / pixdist

  output
}

# Internal function for selecting focal-stimulus outline
outliner <- function(image_i, smooth_i, iterations_i, col, ...) {

  # Plot
  if (attr(image_i, "state") == "raw") {
    plot(image_i, axes = TRUE, col = NULL, ...)
  } else if (attr(image_i, "state") == "colclass") {
    plot(image_i, axes = TRUE, col = NULL, ...)
  }

  # Get coordinates
  xy <- locator(type = "p", col = col, lwd = 2)
  xy <- cbind(xy$x, xy$y)
  xy <- rbind(xy, xy[1, ])

  # Smooth coordinates (Chaikin’s corner cutting)
  if (smooth_i) {
    for (i in seq.int(iterations_i)) {
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
