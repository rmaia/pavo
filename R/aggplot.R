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

aggplot <- function(rspecdata, by, FUN.center = mean, FUN.error = sd, 
    lcol = NULL, shadecol = NULL, alpha = 0.2,
    lty = NULL, xlim = NULL, ylim = NULL, ...){

#take aggregated data
cntplotspecs <- aggspec(rspecdata,by=by, FUN=FUN.center)
errplotspecs <- aggspec(rspecdata, by=by, FUN=FUN.error)

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

# set limits
if (is.null(xlim))
  xlim <- range(wl)

if (is.null(ylim))
  ylim <- range(polygon_data)


# coloring for overlay plot & others

if (length(lty) < ncol(cntplotspecs))
  lty <- rep(lty, ncol(cntplotspecs))

if (length(shadecol) < ncol(cntplotspecs))
  shadecol <- rep(shadecol, ncol(cntplotspecs))
if (any(class(shadecol)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
  shadecol <- spec2rgb(cbind(wl,cntplotspecs))

if (length(lcol) < ncol(cntplotspecs))
  lcol <- rep(lcol, ncol(cntplotspecs))
if (any(class(lcol)=='spec2rgb'))  # this messes up when you give a normal color string; need to look for # or something about hex.
  lcol <- spec2rgb(cbind(wl,cntplotspecs))
   
col_list <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

col_list <- rep(col_list, length=dim(cntplotspecs)[2])

if(is.null(shadecol))
  shadecol <- col_list

if(is.null(lcol))
  lcol <- col_list
 
 shadecol = rgb(t(col2rgb(shadecol))/255, alpha=alpha)
 lcol = rgb(t(col2rgb(lcol))/255)

  plot(cntplotspecs[, 1]~wl, type = 'n', # c(min(rspecdata), max(rspecdata)), 
       xlab = 'Wavelength (nm)', ylab = 'Reflectance (%)', xlim = xlim, ylim = ylim,
       ...)
  polygon(polygon_data[,1]~polygon_wl, col=shadecol[1], border=NA)
  lines(cntplotspecs[,1]~wl, col=lcol[1], lty=lty[1], ...)
  
  if (ncol(cntplotspecs)>1) {
    for (i in 2:ncol(cntplotspecs)){
  polygon(polygon_data[,i]~polygon_wl, col=shadecol[i], border=NA)
  lines(cntplotspecs[,i]~wl, col=lcol[i], lty=lty[i], ...)
  }
  }


}