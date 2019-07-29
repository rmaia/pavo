#' Merge two rspec objects
#'
#' Merges two `rspec` or `data.frame` objects into a single `rspec` object.
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
#' # Load and split dataset into 2 sections
#' data(teal)
#' teal1 <- teal[, c(1, 3:5)]
#' teal2 <- teal[, c(1, 2, 6:12)]
#' teal.mer <- merge(teal1, teal2, by = "wl")
#' head(teal.mer)
#' par(mfrow = c(1, 2))
#' plot(teal.mer)
#' plot(teal)
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @seealso [as.rspec()], [aggspec()]

merge.rspec <- function(x, y, ...) {

## allowed arg has not yet been implemented
# chkDots(..., allowed = "by")

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
