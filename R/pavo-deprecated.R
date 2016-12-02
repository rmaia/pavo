#' Deprecated function(s) in the pavo package
#' 
#' These functions are provided for compatibility with older version of
#' the pavo package.  They may eventually be completely
#' removed.
#' @rdname pavo-deprecated
#' @name pavo-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  tcs
#' @aliases tcs
#' @section Details:
#' \tabular{rl}{
#'   \code{colspace} \tab now a synonym for \code{\link{tcs}}\cr
#' }
#'  

tcs <- function(...){
  .Deprecated('colspace', package='pavo')
  colspace(...)
  }
NULL