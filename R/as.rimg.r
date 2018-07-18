#' Convert data to an rimg object
#'
#' Converts an array containing RGB image data data to an \code{rimg}
#' object.
#'
#' @param object (required) a three-dimensional array containing RGB values.
#' @param name the name(s) of the image(s).
#'
#' @return an object of class \code{rimg} for use in further \code{pavo}
#' functions
#'
#' @export as.rimg is.rimg
#'
#' @examples \dontrun{
#'
#' # Generate some fake reflectance data
#' fake <- array(c(
#' as.matrix(rep(c(0.2, 0.4, 0.6), each = 250)),
#' as.matrix(rep(c(0.4, 0.7, 0.8), each = 250)),
#' as.matrix(rep(c(0.6, 0.1, 0.2), each = 250))),
#' dim = c(750, 750, 3))
#' head(fakedat)
#'
#' # Determine if is rspec object
#' is.rimg(fake)
#'
#' # Convert to rspec object
#' fake2 <- as.rimg(fake)
#' is.rimg(fake2)
#'
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

as.rimg <- function(object, name = "img") {
  if (!inherits(object, "rimg")) {
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
      x
    }

    rescaler <- function(x) {
      if (any(x > 1)) {
        message("Rescaling values to [0,1]")
        for (i in 1:dim(x)[3]) {
          x[, , i] <- x[, , i] / 255
        }
      }
      x
    }

    if (is.list(object)) {

      # Array check
      if (any(unlist(lapply(1:length(object), function(x) !is.array(object[[x]]))))) {
        stop("Images must be an array.")
      }

      # Duplicate channels if grayscale
      for (i in 1:length(object)) {
        if (is.na(dim(object[[i]])[3])) {
          object[[i]] <- replicate(3, object[[i]], simplify = "array")
        }
      }
      
      # 3D maximum
      #object <- lapply(1:length(object), function(j) object[[j]][,,1:3])

      # Rescale RGB to [0,1] if need be
      object <- lapply(1:length(object), function(j) rescaler(object[[j]]))

      # Attributes
      if (length(name) == 1) {
        name <- rep(name, length(object))
      }
      object <- lapply(1:length(object), function(j) attrgiver(object[[j]], name[[j]]))

      # The list itself needs attributes
      class(object) <- c("rimg", "list")
      attr(object, "state") <- "raw"
    } else {

      # Array check
      if (!is.array(object)) {
        stop("Images must be an array.")
      }

      # Duplicate channels if grayscale
      if (is.na(dim(object)[3])) {
        object <- replicate(3, object, simplify = "array")
      }
      
      # 3D maximum
      #object <- object[,,1:3]

      # Rescale RGB to [0,1] if need be
      object <- rescaler(object)

      # Attributes
      object <- attrgiver(object)
    }
  }

  object
}

#' @rdname as.rimg
#' @return a logical value indicating whether the object is of class \code{rimg}
is.rimg <- function(object) {
  inherits(object, "rimg")
}
