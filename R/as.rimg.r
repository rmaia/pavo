#' Convert data to an rimg object
#'
#' Converts an array containing RGB image data data to an \code{rimg}
#' object.
#'
#' @param object (required) a three-dimensional array containing RGB values.
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
#' }
#'
#' @author Thomas E. White \email{thoma.white026@@gmail.com}

as.rimg <- function(object) {
  if (!is.array(object)) {
    stop("object must be an array")
  }

  class(object) <- c("rimg", "array")
  attr(object, "state") <- "raw"

  object
}

#' Check if data is an rimg object.
#'
#' @param object (required) a three-dimensional array containing RGB values.
#' @rdname is.rimg
#' @return a logical value indicating whether the object is of class
#' \code{rimg}

is.rimg <- function(object) {
  inherits(object, "rimg")
}
