#' Plot aggregated reflectance spectra
#' 
#' Combines and plots spectra (by taking the average and the standard deviation, for example) 
#' according to an index or a vector of identities.
#' 
#' @param rspecdata (required) data frame containing the spectra to be manipulated and 
#' plotted. 
#' @param by (required) either a single value specifying the range of spectra within
#' the data frame to be combined (for example, \code{by} = 3 indicates the function
#' will be applied to groups of 3 consecutive columns in the spectra data frame) or
#' a vector containing identifications for the columns in the spectra data frame
#' (in which case the function will be applied to each group of spectra sharing the
#' same identification).
#' @param FUN.center the function to be applied to the groups of spectra, calculating a 
#' measure of central tendency (defaults to \code{mean}).
#' @param FUN.error the function to be applied to the groups of spectra, calculating a 
#' measure of variation (defaults to \code{sd}).
#' @param lcol color of plotted lines indicating central tendency.
#' @param shadecol color of shaded areas indicating variance measure.
#' @param alpha transparency of the shaded areas.
#' @param ... additional graphical parameters to be passed to plot.
#' @return Plot containing the lines and shaded areas of the groups of spectra.
#' @export
#' @examples \dontrun{
#' data(sicalis)
#' bysic <- gsub("^ind[0-9].",'',names(sicalis)[-1])
#' aggplot(sicalis,bysic)
#' aggplot(sicalis,bysic, shade=spec2rgb(sicalis),lcol=1)
#' aggplot(sicalis,bysic,lcol=1, FUN.error=function(x)sd(x)/sqrt(length(x))) }
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}, 
#' Chad Eliason \email{cme16@@zips.uakron.edu}
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds) 
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.

aggplot <- function(rspecdata, by = NULL, FUN.center = mean, FUN.error = sd, 
                    lcol = NULL, shadecol = NULL, alpha = 0.2, ...) {

if (is.numeric(by))
  if (by==1)
    stop('Cannot group single spectra (use plot instead)')

#take aggregated data
cntplotspecs <- aggspec(rspecdata, by = by, FUN = FUN.center)
errplotspecs <- aggspec(rspecdata, by = by, FUN = FUN.error)

# make wavelength vector
wl_index <- which(names(rspecdata)=='wl')
wl_index_cnt <- which(names(cntplotspecs)=='wl')
wl_index_err <- which(names(errplotspecs)=='wl')

if (length(wl_index) > 0) {
  haswl <- TRUE
  wl <- rspecdata[, wl_index]
  cntplotspecs <- as.data.frame(cntplotspecs[,-wl_index_cnt])
  errplotspecs <- as.data.frame(errplotspecs[,-wl_index_err])
} else {
  haswl <- FALSE
  wl <- 1:nrow(rspecdata)
  warning('No wavelengths provided; using arbitrary index values')
}

indexsub <- 1:dim(cntplotspecs)[2]

polygon_data <- sapply(indexsub, function(x) 
			c(cntplotspecs[,x]+errplotspecs[,x], rev(cntplotspecs[,x]-errplotspecs[,x]) )
			)

polygon_wl <- c(wl,rev(wl))

# Set sensible plotting defaults
arg <- list(...)

if (is.null(arg$xlab))
  arg$xlab <- "Wavelength (nm)"
if (is.null(arg$xlim))
  arg$xlim <- range(wl)
if (is.null(arg$ylim))
  arg$ylim <- range(polygon_data)
if (is.null(arg$xlab))
  arg$xlab <- "Wavelength (nm)"
if (is.null(arg$ylab))
  arg$ylab <- "Reflectance (%)"

# coloring for overlay plot & others
if (!is.null(arg$lty))
  lty <- arg$lty

if (is.null(arg$lty))
  lty <- 1

if (length(lty) < ncol(cntplotspecs))
   lty <- rep(lty, ncol(cntplotspecs))

if (length(shadecol) < ncol(cntplotspecs))
  shadecol <- rep(shadecol, ncol(cntplotspecs))
# if (any(class(shadecol)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
#   shadecol <- spec2rgb(cbind(wl,cntplotspecs))

if (length(lcol) < ncol(cntplotspecs))
  lcol <- rep(lcol, ncol(cntplotspecs))
# if (any(class(lcol)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
#   lcol <- spec2rgb(cbind(wl,cntplotspecs))
   
col_list <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

col_list <- rep(col_list, length=dim(cntplotspecs)[2])

if (is.null(shadecol))
  shadecol <- col_list

if(is.null(lcol))
  lcol <- col_list
 
 shadecol = rgb(t(col2rgb(shadecol))/255, alpha=alpha)
 lcol = rgb(t(col2rgb(lcol))/255)

# plot polygons first...

arg$x <- wl
arg$y <- cntplotspecs[, 1]
arg$type <- 'n'

  do.call(plot, arg)

arg$type <- NULL
arg$x <- polygon_wl
arg$y <- polygon_data[, 1]
arg$col <- shadecol[1]  
arg$border <- NA

do.call(polygon, arg)
  
  if (ncol(cntplotspecs)>1) {
    for (i in 2:ncol(cntplotspecs)){
      arg$col <- shadecol[i]
      arg$y <- polygon_data[, i]
      do.call(polygon, arg)
    }
  }
  
# ...then lines (so they are on top)
  arg$border <- NULL
  arg$col <- lcol[1]
  arg$x <- wl
  arg$y <- cntplotspecs[, 1]
  arg$type <- 'l'
  arg$lty <- lty[1]

  do.call(lines, arg)

  if (ncol(cntplotspecs)>1) {
    for (i in 2:ncol(cntplotspecs)){
      arg$y <- cntplotspecs[, i]
      arg$col <- lcol[i]
      arg$lty <- lty[i]
      do.call(lines, arg)
    }
  }

}
