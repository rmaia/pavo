#' Convert data to an rimg object
#'
#' Converts an array of RGB values, a `cimg` object, or a `magick-image` object,
#' to an `rimg` object.
#'
#' @param object (required) a three-dimensional array containing RGB values.
#' @param name the name(s) of the image(s).
#'
#' @return an object of class `rimg` for use in further `pavo` functions
#'
#' @export
#'
#' @examples
#'
#' # Generate some fake image data
#' fake <- array(c(
#'   as.matrix(rep(c(0.2, 0.4, 0.6), each = 250)),
#'   as.matrix(rep(c(0.4, 0.7, 0.8), each = 250)),
#'   as.matrix(rep(c(0.6, 0.1, 0.2), each = 250))
#' ),
#' dim = c(750, 750, 3)
#' )
#'
#' # Inspect it
#' head(fake)
#'
#' # Determine if is rimg object
#' is.rimg(fake)
#'
#' # Convert to rimg object and check again
#' fake2 <- as.rimg(fake)
#' is.rimg(fake2)
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#'
as.rimg <- function(object, name = "img") {
  UseMethod("as.rimg")
}

#' @rdname as.rimg
#'
#' @export
#'
as.rimg.default <- function(object, name = "img") {

  if (is.rimg(object)) {
    # Just output the original, if already 'rimg'
    return(object)
  }

  attrgiver <- function(x, name2 = name) {
    # Attributes
    class(x) <- c("rimg", "array")
    attr(x, "state") <- "raw"
    attr(x, "imgname") <- name2
    attr(x, "px_scale") <- NA
    attr(x, "raw_scale") <- NA
    attr(x, "k") <- NA
    attr(x, "outline") <- NA
    attr(x, "colnames") <- NA
    attr(x, "tag_loc") <- NA
    x
  }

  rescaler <- function(x) {
    if (any(x > 1)) {
      message("Rescaling values to [0,1]")
      x <- x / 255
    }
    x
  }

  # Control flow for multi-images
  if (!inherits(object, "list")) {
    object2 <- list(object)
  } else {
    object2 <- object
  }

  # Is it already colour-classified by the user?
  # Tricky to distinguish between single-dimension (greyscale) RGB & a
  # colour-classified matrix. Best I've got atm.
  is.whole <- function(x) {
    all(is.numeric(x), floor(x) == x)
  }
  if (inherits(object2[[1]], "matrix") && max(object2[[1]]) < 30 && is.whole(object2[[1]])) {
    colclass <- TRUE
  } else {
    colclass <- FALSE
  }

  if (!colclass) {

    # Array check
    if (any(unlist(lapply(seq_along(object2), function(x) !is.array(object2[[x]]))))) {
      stop("Images must be an array.")
    }

    # Duplicate channels if grayscale
    for (i in seq_along(object2)) {
      if (is.na(dim(object2[[i]])[3])) {
        object2[[i]] <- replicate(3, object2[[i]], simplify = "array")
      }
    }

    # 3D maximum
    object2 <- lapply(seq_along(object2), function(j) object2[[j]][, , 1:3])

    # Rescale RGB to [0,1] if need be
    object2 <- lapply(object2, rescaler)

    # Attributes
    if (length(name) == 1) {
      name <- rep(name, length(object2))
    }
    object2 <- lapply(seq_along(object2), function(j) attrgiver(object2[[j]], name[[j]]))

    # The list itself needs attributes
    class(object2) <- c("rimg", "list")
    attr(object2, "state") <- "raw"
  } else if (colclass) {

    # Attributes
    if (length(name) == 1) {
      name <- rep(name, length(object2))
    }
    object2 <- lapply(seq_along(object2), function(j) attrgiver(object2[[j]], name[[j]])) # names
    for (i in seq_along(object2)) attr(object2[[i]], "state") <- "colclass" # classification state
    for (i in seq_along(object2)) attr(object2[[i]], "k") <- length(table(object2[[i]])) # kcols
    for (i in seq_along(object2)) attr(object2[[i]], "class") <- c("rimg", "matrix") # class
    for (i in seq_along(object2)) attr(object2[[i]], "colnames") <- data.frame(name = seq_along(table(object2[[i]]))) # colour-category names (in progress)
    for (i in seq_along(object2)) {
      attr(object2[[i]], "classRGB") <- data.frame(
        R = rep(NA, length(table(object2[[i]]))),
        G = rep(NA, length(table(object2[[i]]))),
        B = rep(NA, length(table(object2[[i]])))
      )
    }
    # The list itself needs attributes
    class(object2) <- c("rimg", "list")
    attr(object2, "state") <- "colclass"
  }
  # Output
  if (!inherits(object, "list") || length(object) == 1) {
    object2[[1]]
  } else {
    object2
  }

}

#' @rdname as.rimg
#'
#' @export
as.rimg.cimg <- function(object, name = "img") {
  as.rimg.default(drop(as.array(object)), name = name)
}

#' @importFrom magick image_flop image_rotate image_data
#'
#' @export
#'
#' @noRd
`as.rimg.magick-image` <- function(object, name = "img") {
  object <- image_rotate(object, 90)
  object <- image_flop(object)
  suppressMessages(as.rimg.default(lapply(object, function(img) as.integer(image_data(img))), name = name))
}

#' @rdname as.rimg
#'
#' @export
#'
#' @return a logical value indicating whether the object is of class `rimg`
is.rimg <- function(object) {
  inherits(object, "rimg")
}

#' Convert images between class rimg and cimg or magick-image
#'
#' Conveniently convert single objects of class `rimg` to class `cimg` (from the
#' package `imager`) or `magick-image` (from the package `magick`), both of which
#' contains a suite of useful image-processing capabilities.
#'
#' @param image an object of class `rimg`
#' @inheritParams as.rimg
#'
#' @return an image of the specified class
#'
#' @note Attributes (e.g. scales, color-classes) will not be preserved following
#' conversion from class `rimg`, so it's best to use early in the analysis workflow.
#'
#' @examples
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = "pavo"))
#'
#' # From class rimg to cimg
#' papilio_cimg <- rimg2cimg(papilio)
#' class(papilio_cimg)
#'
#' # From class rimg to magick-image
#' papilio_magick <- rimg2magick(papilio)
#' class(papilio_magick)
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#' @name img_conversion
#'
NULL

#' @rdname img_conversion
#'
#' @export
rimg2cimg <- function(image) {
  ## Check for imager
  if (!requireNamespace("imager", quietly = TRUE)) {
    stop("Package \"imager\" needed for conversion to cimg. Please install it.",
      call. = FALSE
    )
  }
  image <- suppressWarnings(imager::as.cimg(image, cc = 3))
  image
}

#' @rdname img_conversion
#'
#' @importFrom magick image_read image_flop image_rotate
#'
#' @export
rimg2magick <- function(image) {
  if (inherits(image, "list")) {
    output <- do.call(c, lapply(image, image_read))
  } else {
    output <- image_read(image)
  }
  output <- image_rotate(output, 90)
  image_flop(output)
}
