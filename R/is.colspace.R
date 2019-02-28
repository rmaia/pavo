#' Test is object is of class 'colspace'
#'
#' @param object an R object
#'
#' @return a logical value indicating whether the object is of class \code{colspace}
#'
#' @export
#'
#' @seealso \code{\link{colspace}}
#'
is.colspace <- function(object) {
  inherits(object, "colspace")
}
