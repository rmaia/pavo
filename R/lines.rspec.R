#' Add lines to an existing spectrum plot
#'
#' Connecting reflectance spectra datapoints with line segments.
#'
#' @method lines rspec
#' @param x (required) a data frame, possibly an object of class \code{rspec},
#' with a column with wavelength data, named `wl', and the remaining column containing
#' spectra to plot.
#' @param select specification of which spectra to plot. Can be a numeric vector or 
#' factor (e.g., \code{sex=='male'})
#' @param ... additional arguments passed to lines
#' @examples \dontrun{
#' data(teal)
#' plot(teal, select = 2:4, col = 'red')
#' lines(teal, select = 5:8, col = 'blue')
#' }
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @seealso \code{\link{plot}}, \code{\link{lines}}
#' @export lines.rspec

lines.rspec <- function(x, select = NULL, type = 'overlay', ...) {
  type <- match.arg(type)

  # make wavelength vector
  wl_index <- which(names(x)=='wl')
  if (length(wl_index) > 0) {
    haswl <- TRUE
    wl <- x[, wl_index]
  } else {
    haswl <- FALSE
    wl <- 1:nrow(x)
    warning('No wavelengths provided; using arbitrary index values')
    }

  # subset based on indexing vector
  if (is.logical(select))
    select <- which(select=='TRUE')
  if (is.null(select)&haswl==TRUE)
    select <- (1:ncol(x))[-wl_index]
  if (is.null(select)&haswl==FALSE)
    select <- 1:ncol(x)

  x <- as.data.frame(x[select])  # CE: removed comma before select

  arg <- list(...)

  # Set defaults
  if (is.null(arg$xlab))
    arg$xlab <- "Wavelength (nm)"
  if (is.null(arg$xlim))
    arg$xlim <- range(wl, na.rm=TRUE)


  # coloring for overlay plot & others
  if (length(arg$col) < ncol(x)) {
    arg$col <- rep(arg$col, ncol(x))
    arg$col <- arg$col[1:ncol(x)]
   }

  if (any(names(arg$col)%in%names(x))) {
    arg$col <- arg$col[select-1]
  }


  # overlay different spec curves
  if (type=='overlay') {

    if (is.null(arg$ylim))
      arg$ylim <- range(x, na.rm=TRUE)
    if (is.null(arg$ylab))
      arg$ylab <- "Reflectance (%)"
    arg$type <- 'l'
    arg$x <- wl
    arg$y <- x[, 1]
    col <- arg$col
    arg$col <- col[1]

    do.call(lines, arg)

    if (ncol(x) > 1) {
      for (i in 2:ncol(x)) {
        arg$col <- col[i]
        arg$y <- x[, i]
        do.call(lines, arg)
      }
    }
  }
}
