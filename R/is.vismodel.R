#' @return a logical value indicating whether the object is of class \code{vismodel}
is.vismodel <- function(object) {
  inherits(object, "vismodel")
}
