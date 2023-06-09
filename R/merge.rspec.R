#' Merge two rspec objects
#'
#' Merges two `rspec` objects into a single `rspec` object.
#'
#' @param x,y (required) `rspec` objects to merge.
#' @param ... additional class arguments.
#' @return an object of class `rspec` for use with `pavo` functions.
#' Will use `by = "wl"` if unspecified, or automatically append `wl` to the
#' `by` argument if one is specified.
#'
#' @export
#'
#' @examples
#'
#' # Load angle-resolved reflectance data for a green-winged teal, and
#' # split it in two
#' data(teal)
#' teal1 <- teal[, c(1, 2:5)]
#' teal2 <- teal[, c(1, 6:13)]
#'
#' # Merge the two split datasets back into one, with a shared 'wl' column
#' teal.mer <- merge(teal1, teal2, by = "wl")
#'
#' # Examine the results, and compare the original to the (identical)
#' # reconstructed version
#' plot(teal.mer)
#' plot(teal)
#' identical(teal.mer, teal)
#' 
#' # Or an equivalent method, which also allows for the merging of more than one rspec
#' # object at a time (simply add further objects to the list())
#' teal.mer2 <- do.call(merge, list(teal1, teal2))
#' 
#' # Check equivalence
#' identical(teal.mer2, teal)
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @seealso [as.rspec()], [aggspec()]

merge.rspec <- function(x, y, ...) {

  if (!all(is.rspec(x), is.rspec(y))) {
    stop("One or more invalid rspec objects")
  }

  if (!all("wl" %in% names(x), "wl" %in% names(y))) {
    stop("Cannot find valid 'wl' column in one or both input objects")
  }

  arg <- list(...)
  arg$by <- c("wl", arg$by)
  arg$x <- x
  arg$y <- y

  res <- do.call(merge.data.frame, arg)
  class(res) <- c("rspec", "data.frame")
  res
}
