#' Process images
#'
#' Specify scales, resize, and/or define focal objects within images.
#'
#' @param image (required) image data. Either a single image array, or a number of images
#' stored in a list. Preferably the result of [getimg()].
#' @param resize an integer specifying a percentage for resizing images, if so desired.
#' E.g. 50 to half the size of an image, 200 to double it.
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
#' @param reclass interactively specify an area on a colour-classified image that is
#' to be reclassified as the numeric value provided. e.g. when `reclass = 1`, the user
#' will be asked to select a polygon on the image, within which all colour-category values will be
#' changes to `1`.
#' @param col the color of the marker points and/or line, when using interactive options.
#' @param smooth should the polygon specified when `outline = TRUE` be smoothed
#' using Chaikin's corner-cuting algorithm? Defaults to `FALSE`.
#' @param iterations the number of smoothing iterations, when `smooth = TRUE`.
#' Defaults to `1`.
#' @param obj_dist,obj_width,eye_res blur the image to model the visual acuity of non-human animals
#' as per Caves & Johnsen (2018)'s AcuityView 2.0 algorithm. The procedure requires three arguments;
#' obj_dist is the real-world distance between the viewer and the focal object in the image in the image,
#' obj_width is the real-world width of the entire image; eye_res is the minimum resolvable angle of the viewer
#' in degrees. All three arguments are numeric, and any units of measurement are suitable for
#' obj_dist and obj_width, but they must match. Note that this is the more flexible v2.0 implementation meaning
#' that any rectangular image is suitable; it need not be square with dimensions a power of 2.
#' If using this capability, please cite Caves & Johnsen (2018), as per the included reference,
#' and see note below.
#' @param plotnew should plots be opened in a new window? Defaults to `FALSE`.
#' @param ... additional graphical parameters. Also see [par()].
#'
#' @return an image, or list of images, for use in further
#' `pavo` functions.
#'
#' @importFrom magick image_rotate image_resize
#' @importFrom grDevices dev.new dev.off
#' @importFrom graphics lines
#'
#' @note There are several caveats that should be considered when using the AcuityView
#' algorithm. First and foremost, the converted image is not what the animal actually sees.
#' For example, it does not account for edge enhancement and other processing by the retina and
#' brain that may alter an image. It does, however, show what spatial information
#' can be detected and then processed by the visual system. Second, the converted
#' image is static, which does not allow one to assess how movement may reveal the
#' presence of an otherwise indiscernible object. Third, AcuityView makes several
#' assumptions about the Modulation Transfer Function (MTF), which describes how
#' the optical system affects image contrast as a function of the level of detail.
#' These assumptions include that the MTF is constant over the region of the retina that views the
#' scene, is circularly symmetrical, and is wavelength independent. For a full discussion and details,
#' please do read Caves & Johnsen (2018).
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Interactively add a scale to a single image
#'   papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
#'   papilio <- procimg(papilio, scaledist = 10)
#'
#'   # Interactively assign individual scales to each image,
#'   # after slightly reducing their size (to 90% of original).
#'   snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#'   snakes <- procimg(snakes, scaledist = c(10, 14), resize = 90)
#'
#'   # Model the appearance of a butterfly given the reduced visual acuity of another
#'   # animal viewer as per the AcuityView algorithm. Here our butterfly is 60 cm away,
#'   # the image width is 10 cm, and the minimum resolvable angle of the viewer is 0.2-degrees.
#'   tiger <- getimg(system.file("testdata/images/tiger.png", package = "pavo"))
#'   tiger_acuity <- procimg(tiger, obj_dist = 60, obj_width = 10, eye_res = 0.2)
#' }
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @references Caves, E. M., & Johnsen, S. (2018). AcuityView: An r package
#' for portraying the effects of visual acuity on scenes observed by an animal.
#' Methods in Ecology and Evolution, 9(3), 793-797 \doi{10.1111/2041-210X.12911}.
#' @references Chaikin, G. 1974. An algorithm for high speed curve generation.
#' Computer Graphics and Image Processing 3, 346-349.

procimg <- function(image, resize = NULL, rotate = NULL, scaledist = NULL,
                    outline = FALSE, reclass = NULL, smooth = FALSE, iterations = 1L,
                    col = "red", obj_dist = NULL, obj_width = NULL, eye_res = NULL,
                    plotnew = FALSE, ...) {
  ## ------------------------------ Checks ------------------------------ ##

  ## Class
  if (!inherits(image, "rimg")) {
    message("Attempting to coerce image to class rimg.")
    image <- as.rimg(image)
  }

  ## Options
  if (is.null(scaledist) && !outline && is.null(resize) && is.null(rotate) && is.null(reclass) &&
    is.null(obj_dist) && is.null(obj_width) && is.null(eye_res)) {
    stop("No options selected.", call. = FALSE)
  }

  ## If it's a single image, store it in a list for processing convenience,
  ## before converting it back at the end
  if (!inherits(image, "list")) {
    image <- list(image)
  }

  ## ------------------------------ Main ------------------------------ ##

  ## Resize and Rotate ##
  if (attr(image[[1]], "state") == "colclass" && is.numeric(resize)) {
    message("Cannot resize colour-classified images.")
    resize <- NULL
  }
  if (attr(image[[1]], "state") == "colclass" && is.numeric(rotate)) {
    message("Cannot rotate colour-classified images.")
    rotate <- NULL
  }
  if (is.numeric(resize) || is.numeric(rotate)) {
    imgnames <- lapply(image, attr, "imgname")
    image <- rimg2magick(image)
    if (is.numeric(rotate)) {
      image <- image_rotate(image, rotate)
    }
    if (is.numeric(resize)) {
      size <- paste0(resize, "%") # magick geometry string
      image <- image_resize(image, size, "Quadratic")
    }
    image <- as.rimg(image, imgnames)
    if (!inherits(image, "list")) {
      image <- list(image)
    }
  }

  ## Scale ##
  if (is.numeric(scaledist)) {
    # Formatting
    if (length(scaledist) == 1) {
      scaledist <- as.list(rep(scaledist, length(image)))
    }
    if (length(scaledist) > 1 && length(scaledist) != length(image)) {
      stop(
        "Number of scales provided greater than one, but unequal to the the number of images. Provide a single scale to be recycled, or one per image.",
        call. = FALSE
      )
    }

    if (plotnew) dev.new(noRStudioGD = TRUE)
    message("Scale calibration: Select both ends of the scale, images will progress automatically.")
    for (i in seq_along(image)) {
      attr(image[[i]], "px_scale") <- scaler(
        image_i = image[[i]],
        scaledist_i = scaledist[[i]], col = col, ...
      )
      attr(image[[i]], "raw_scale") <- scaledist[[i]]
    }
    if (plotnew) dev.off()
  }

  ## Model acuity
  if (!is.null(c(obj_dist, obj_width, eye_res))) {
    # Require all arguments
    if (!(is.numeric(obj_dist) && is.numeric(obj_width) && is.numeric(eye_res))) {
      warning(
        "Numeric values for each of obj_dist, obj_width, and eye_res ",
        "must be specified for acuity modelling. Skipping acuity modelling.",
        call. = FALSE
      )
    }
    # Raw images only
    if (attr(image[[1]], "state") == "colclass") {
      warning("Acuity modelling can only be run on non-colour-classified (raw) images. Skipping acuity modelling.", call. = FALSE)
    }
    # Model
    if ((is.numeric(obj_dist) && is.numeric(obj_width) && is.numeric(eye_res) && attr(image[[1]], "state") == "raw")) {
      message(
        "When using the AcuityView algorithm, please remember to read (for full discussion and caveats) and cite: ",
        "Caves EM & Johnsen S (2018). AcuityView: An r package for portraying ",
        "the effects of visual acuity on scenes observed by an animal. Methods ",
        "in Ecology and Evolution, 9(3), 793-797."
      )
      for (i in seq_along(image)) {
        image[[i]] <- acuityview_pad(image[[i]], obj_dist, obj_width, eye_res)
      }
    }
  }

  ## Select outline ##
  if (outline) {
    if (plotnew) dev.new(noRStudioGD = TRUE)
    for (i in seq_along(image)) {
      message("Select the outline of focal stimulus, and press [esc] when complete.
                The first and last points will be automatically connected.")
      attr(image[[i]], "outline") <- outliner(image[[i]], smooth, iterations, col = col, ...)
    }
    if (plotnew) dev.off()
  }

  ## Manual classification correction ##
  if (attr(image[[1]], "state") == "raw" && !is.null(reclass)) {
    message("Cannot fix colour-classification on unclassified images.")
    reclass <- NULL
  }
  if (!is.null(reclass)) {
    if (plotnew) dev.new(noRStudioGD = TRUE)
    for (i in seq_along(image)) {
      message("Select the area to be reclassified, and press [esc] when complete.
              The first and last points will be automatically connected.")
      patch_poly <- outliner(image[[i]], smooth, iterations, col = col, ...)
      if (!is.null(patch_poly)) {
        image[[i]] <- polymask(image[[i]], patch_poly, "inside", reclass)
      }
    }
    if (plotnew) dev.off()
  }

  if (length(image) == 1) {
    image <- image[[1]]
  }

  image
}

# Internal function for calibrating image scale
#' @importFrom graphics rasterImage locator
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
  if (!is.null(xy)) {
    xy <- cbind(xy$x, xy$y)
    xy <- rbind(xy, xy[1, ])

    # Smooth coordinates (Chaikin's corner cutting)
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
  } else {
    NULL
  }
}
