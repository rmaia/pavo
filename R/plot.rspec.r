#' Plot spectra
#'
#' Plots reflectance spectra in different arrangements.
#'
#' @S3method plot rspec
#' @method plot rspec
#' @param x (required) a data frame, possibly an object of class \code{rspec},
#' with a column with wavelength data, named 'wl', and the remaining column containing
#' spectra to plot.
#' @param select specification of which spectra to plot. Can be a numeric vector or 
#' factor (e.g., \code{sex=='male'})
#' @param type what type of plot should be drawn. Possibilities are: 
#' \itemize{
#'  \item \code{overlay} (default) for plotting multiple spectra in a single panel with 
#' a common y-axis
#'  \item \code{stack} for plotting multiple spectra in a vertical arrangement
#'  \item \code{heatmap} for plotting reflectance values by wavelength and a third variable 
#'        (\code{varying})
#' }
#' @param varying a numeric vector giving values for y-axis in \code{heatplot}
#' @param n number of bins with which to interpolate colors and \code{varying} for the 
#' heatplot.
#' @param ... additional arguments passed to plot (or image for \code{'heatmap'}).
#' @examples \dontrun{
#' data(teal)
#' plot(teal, type = 'overlay')
#' plot(teal, type = 'stack')
#' plot(teal, type = 'heatmap')}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @seealso \code{\link{spec2rgb}}, \code{\link{image}}, \code{\link{plot}}

# TODO: add argument for padding region between x in stack plot

plot.rspec <- function(x, select = NULL, type = c('overlay', 'stack', 'heatmap'), 
                       varying = NULL, n = 100, ...) {

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

# heat plot
if (type=='heatmap') {

  if (is.null(arg$xlab))
    arg$xlab <- "Wavelength (nm)"
  if (is.null(arg$ylab))
    arg$ylab <- "Index"
  if (is.null(varying)) { 
    varying <- 1:ncol(x)
    print("No varying vector supplied; using arbitrary values")
  }
  if (is.null(arg$ylim))
    arg$ylim <- range(varying, na.rm=TRUE)
  if (is.null(arg$col)==1) {
    jc <- colorRampPalette( rev(c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", 
                                  "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", 
                                  "#66C2A5", "#3288BD", "#5E4FA2")))
    arg$col <- jc(n)
  } else {
  	jc <- colorRampPalette(arg$col)
  	arg$col <- jc(n)
  	}

  Index <- approx(varying, n = n)$y
  dat <- sapply(1:nrow(x), function(z){approx(x = varying, y = x[z, ], 
                n = n)$y})

  arg$x <- wl
  arg$y <- Index
  arg$z <- t(dat)

  do.call(image, arg)
}

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

  do.call(plot, arg)

  if (ncol(x) > 1) {
    for (i in 2:ncol(x)) {
      arg$col <- col[i]
      arg$y <- x[, i]
      do.call(lines, arg)
    }
  }
}

# stack curves along y-axis
if (type=='stack') {

  arg$type <- 'l'
  if (is.null(arg$ylab))
    arg$ylab <- "Cumulative reflectance (arb. units)"

  x2 <- as.data.frame(x[, c(ncol(x):1)])
  if (length(select)==1){
    y <- max(x2)} else {
    y <- apply(x2, 2, max)
  }
  ym <- cumsum(y)
  ymins <- c(0, ym[-length(ym)])

  arg$x <- wl
  arg$y <- x2[, 1]
  if (is.null(arg$ylim))
    arg$ylim <- c(0, sum(y))

  col <- rev(arg$col)
  arg$col <- col[1]
  do.call(plot, arg)
  if (ncol(x2)>1) {
    for (i in 2:ncol(x2)) {
      arg$y <- x2[, i] + ymins[i]
      arg$col <- col[i]
      do.call(lines, arg)
    }
  }

  yend <- tail(x2, 1)
  yloc <- ymins + yend
  axis(side=4, at=yloc, labels=rev(select), las=1)
#  abline(h=ymins, lty=3)
}
}
