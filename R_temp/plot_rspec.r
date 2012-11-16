#' Plot spectra
#'
#' Plots reflectance spectra in different arrangements.
#'
#' @param specs (required) an \code{rspec} object containing spectra to plot
#' @param select specification of which spectra to plot. Can be a numeric vector or 
#' factor (e.g., \code{sex=='male'})
#' @param type what type of plot should be drawn. Possibilities are: 
#' \itemize{
#'  \item @param overlay for plotting multiple spectra in a single panel with a common y-axis
#'  \item @param stack for plotting multiple spectra in a vertical arrangement
#'  \item @param heatmap for plotting reflectance values by wavelength and a third variable 
#'        (\code{varying})
#'  \item @param grid for plotting spectra in separate panels
#' }
#' @param cols the number of columns used in \code{grid} plots
#' @param varying # a numeric vector giving values for third variable used in 
#' \code{heatplot}
#' @param n number of bins with which to interpolate \code{varying}
#' @param col color of the spec curves. User can either provide a single color, a vector 
#' of colors (e.g., code\{heat.colors(25)} for heaplots) or a \code{spec2rgb} object. 
#' For the latter, picking spectra with \code{select} will also subset the color vector.
#' @param xlim a numeric vector giving the lower an upper limits for the x-axis
#' @param ylim a numeric vector giving the lower an upper limits for the y-axis
#' Does not apply for \code{type = 'heatmap'}.
#' @param lwd width of lines 
#' @export
#' @examples \dontrun{
#' #INCLUDE EXAMPLE}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

# TODO: add argument for padding region between specs in stack plot
# TODO: add labels to curves along y-axis for stacked plot (ideas anyone?)
# TODO: figure out way to label y-axis in heatplot (ideas?)

plot.rspec <- function(specs, select = NULL, type = c('overlay', 'stack', 'heatmap',     
                       'grid'), cols = 2, varying = NULL, n = 100, col = 'black', 
                       xlim = NULL, ylim = NULL, lwd = 1) {

old.par <- par(no.readonly = TRUE)  # all par settings that could be set
type <- match.arg(type)

# make wavelength vector
wl_index <- which(names(specs)=='wl')
if (length(wl_index > 0)) {
  wl <- specs[, wl_index]
  specs <- as.data.frame(specs[, -wl_index])
} else if (length(wl_index==0)) {
  wl <- 1:nrow(specs)
  specs <- as.data.frame(specs)
  warning('No wavelengths provided; using arbitrary index values')
}

# subset based on indexing vector
if (is.logical(select))
  select <- which(select=='TRUE')
else if (is.null(select))
  select <- 1:ncol(specs)
specs <- as.data.frame(specs[, (select-1)])

# set limits
if (is.null(xlim))
  xlim <- range(wl)
else xlim <- xlim

if (is.null(ylim))
  ylim <- range(specs)
else ylim <- ylim


# coloring
if (length(col)==1)
  col <- rep(col, ncol(specs))
if (any(class(col)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
  col <- col[select-1]
else {
  col <- col
}

# heat plot
if (type=='heatmap') {
  if (is.null(varying)) { 
    varying <- 1:ncol(specs)
    print("No varying vector supplied; using arbitrary values")
  }
    varying <- varying
  dat <- sapply(1:nrow(specs), function(z){approx(x = varying, y = specs[z, ], 
                n = n)$y})
  image(x = wl, y = approx(varying, n = n)$y, z = t(dat), col = col,
        xlab = 'Wavelength (nm)', xlim = xlim)
}

# overlay different spec curves
if (type=='overlay') {
  plot(specs[, 1]~wl, type = 'l', # c(min(specs), max(specs)), 
       xlab = 'Wavelength (nm)', ylab = 'Reflectance (%)', xlim = xlim, ylim = ylim,
       col = col[1], lwd = lwd)
  if (ncol(specs)>1) {
    for (i in 2:ncol(specs))
      lines(specs[, i]~wl, col=col[i], lwd = lwd)
  }
}

# stack curves along y-axis
if (type=='stack') {
  specs2 <- sapply(1:ncol(specs), function(z){specs[, z] - min(specs[, z])})
  ym <- apply(specs2, 2, max)  
  plot(specs2[, 1]~wl, type='l', xlim = xlim, ylim = c(0, sum(ym)), 
       xlab = 'Wavelength (nm)', ylab = 'Reflectance (arb. units)', col = col[1], 
       lwd = lwd)
  if (ncol(specs)>1) {
    for (i in 2:ncol(specs)) 
      lines((specs2[, i] + cumsum(ym)[i - 1])~wl, col = col[i], lwd = lwd)
    }
}

# arrange plots in a grid
if (type=='grid') {
  rows <- ceiling(ncol(specs)/cols)
  par(mfrow = c(rows, cols))
  for (i in 1:ncol(specs)) {
    plot(specs[, i]~wl, type = 'l', main = names(specs)[i],
         xlab = "Wavelength (nm)", ylab = "Reflectance (%)", xlim = xlim, ylim = ylim,
         col = col[i], lwd = lwd)  # fix ylim
  }
}

par(old.par)  # return settings to previous

}
