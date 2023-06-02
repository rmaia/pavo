#' Merge rspec objects
#'
#' Merges two or more `rspec` objects into a single `rspec` object.
#' 
#' @param x (required) an `rspec` object
#' @param ... (required) further `rspec` objects to merge by their wavelength ('wl') values, 
#' and any additional parameters.
#' @return an object of class `rspec` comprising multiple spectra with a single
#' shared wavelength (`wl`) column, for use with `pavo` functions.
#'
#' @export
#'
#' @examples
#'
#' # Load angle-resolved reflectance data for a green-winged teal, and
#' # split it into three objects, each with a 'wl' column
#' data(teal)
#' teal1 <- teal[, c(1, 2:5)]
#' teal2 <- teal[, c(1, 6:8)]
#' teal3 <- teal[, c(1, 9:13)]
#'
#' # Merge the three split datasets back into one, with a shared 'wl' column
#' teal.mer <- merge(teal1, teal2, teal3)
#'
#' # Examine the results, and compare the original to the (identical)
#' # reconstructed version
#' plot(teal.mer)
#' plot(teal)
#' identical(teal.mer, teal)
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @seealso [as.rspec()], [aggspec()]

merge.rspec <- function(x, ...) {
  
  # Separate the rspec objects from other arguments
  args <- list(...)
  rspec_args <- Filter(is.rspec, args)
  other_args <- Filter(Negate(is.rspec), args)
  
  # If there's only one rspec argument, handle it as before
  if(length(rspec_args) == 1) {
    y <- rspec_args[[1]]
    
    if (!all(is.rspec(x), is.rspec(y))) {
      stop("One or more invalid rspec objects")
    }
    
    if (!all("wl" %in% names(x), "wl" %in% names(y))) {
      stop("Cannot find valid 'wl' column in one or both input objects")
    }
    
    other_args$by <- c("wl", other_args$by)
    other_args$x <- x
    other_args$y <- y
    
    res <- do.call(merge.data.frame, other_args)
    class(res) <- c("rspec", "data.frame")
    
  } else {
    # If there are multiple rspec arguments, add the first to the list
    rspec_args <- c(list(x), rspec_args)
    
    if (any(sapply(rspec_args, function(x) !is.rspec(x) || !("wl" %in% names(x))))) {
      stop("One or more invalid rspec objects, or 'wl' column is missing in one or more objects")
    }
    
    other_args$by <- "wl"
    
    res <- Reduce(function(x, y) {
      other_args$x <- x
      other_args$y <- y
      do.call(merge.data.frame, other_args)
    }, rspec_args)
    
    class(res) <- c("rspec", "data.frame")
  }
  
  res
}
