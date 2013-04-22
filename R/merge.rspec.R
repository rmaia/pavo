#' Merge two rspec objects
#'
#' Merges two \code{rspec} or \code{data.frame} objects into a single \code{rspec} object
#'
#' @S3method merge rspec
#' @method merge rspec
#' @param x,y (required) two data frames (or \code{rspec} objects) to merge
#' @param by wavelength column name (defaults to \code{"wl"})
#' @param ... additional class arguments
#' @return an object of class \code{rspec} for use with \code{pavo} functions
#'
#' @examples \dontrun{
#'
#' # Load and split dataset into 2 sections
#' data(teal)
#' teal1 <- teal[, c(1, 3:5)]
#' teal2 <- teal[, c(1, 2, 6:12)]
#' teal.mer <- merge(teal1, teal2, by='wl')
#' head(teal.mer)
#' par(mfrow=c(1, 2))
#' plot(teal.mer)
#' plot(teal)
#' }
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @seealso \code{\link{as.rspec}}, \code{\link{aggspec}}

merge.rspec <- function(x, y, by='wl', ...) {
  if (!(is.rspec(x)&&is.rspec(y))) {stop("One or more invalid rspec objects")}
  if (!(any(names(x)=='wl')&any(names(y)=='wl'))) {
    stop("Cannot find valid 'wl' column in one or both input objects")
  }
  res <- merge.data.frame(x, y, ...)
  class(res) <- c("rspec", "data.frame")
  res
}
