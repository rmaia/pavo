#' Deprecated function(s) in the pavo package
#'
#' These functions are provided for compatibility with older version of
#' the pavo package. They may eventually be completely removed.
#' @rdname pavo-deprecated
#' @name pavo-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  tcs segclass getspecf getspecf2
#' @aliases tcs segclass getspecf getspecf2
#' @section Details:
#' \tabular{rl}{
#'   \code{tcs} \tab now a synonym for \code{\link{colspace}}\cr
#'   \code{segclass} \tab now a synonym for
#'   \code{\link[=vismodel]{vismodel(..., visual = "segment")}}\cr
#'   \code{getspecf} \tab removed, use \code{\link{getspec}}\cr
#'   \code{getspecf2} \tab removed, use \code{\link{getspec}}\cr
#' }
#'

tcs <- function(...) {
  .Deprecated("colspace", package = "pavo")
  colspace(...)
}

segclass <- function(rspecdata, range=c(300, 700)) {
  .Deprecated("vismodel", msg = paste(sQuote("segclass"), " is deprecated.\n", "Instead, use ", sQuote("vismodel"), " (with argument ", sQuote(paste("visual = ", sQuote("segment"))), ") and ", sQuote("colspace"), ".\n", "See help(", dQuote("Deprecated"), ") and help(", dQuote("pavo-deprecated"), ").", sep = ""))
  vismodel(rspecdata, visual = "segment")
}

getspecf <- function(...) {
  .Deprecated("getspec", package = "pavo")
}

getspecf2 <- function(...) {
  .Deprecated("getspec", package = "pavo")
}

NULL
