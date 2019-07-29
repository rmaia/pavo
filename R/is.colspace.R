#' Test if object is of class 'colspace'
#'
#' @param object an R object
#'
#' @return a logical value indicating whether the object is of class `colspace`
#'
#' @export
#'
#' @seealso [colspace()]
#'
is.colspace <- function(object) {
  inherits(object, "colspace")
}
