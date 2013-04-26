#' Process spectra
#'
#' Applies normalization and/or smoothing to spectra for further analysis or plotting
#'
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec},
#' with a column with wavelength data, named 'wl', and the remaining column containing
#' spectra to process.
#' @param opt what type of processing options to apply. User can select multiple options
#'            by providing a vector. Possibilites are:
#' \itemize{
#'	\item \code{"none"} does not perform any processing (default).
#'	\item \code{"smooth"} applies LOESS smoothing to each spectrum using 
#'                      \code{\link{loess.smooth}}. Optimal smoothing parameter
#'                      can be assessed by using \code{\link{plotsmooth}}.
#' 	\item \code{"minimum"} subtracts the minimum from each individual spectra.
#' 	\item \code{"maxmimum"} divides each spectrum by its maximum value
#' 	\item \code{"sum"} divides each spectrum by summed values.
#' 	\item \code{"bin"} bins each spectrum into specified wavelength ranges. User should
#'									 specify.
#'  \item \code{"center"} centers individual spectra by subtracting mean reflectance from 
#'                      all values.
#' }
#'
#' @param fixneg how to handle negative values. Possibilities are:
#' \itemize{ 
#'	\item \code{"none"} does not perform negative value correction (default).
#'    \item \code{"zero"} sets all negative values to zero.
#'    \item \code{"addmin"} adds the absolute value of the maximally negative values of each
#'                           spectra to the reflectance at all other wavelengths (setting
#'                           the minimum value to zero, but scaling other values accordingly).
#' }
#' @param span sets the smoothing parameter used by \code{loess.smooth}
#' @param bins sets the number of equally sized wavelength bins for \code{opt="bin"}
#' @return A data frame of class \code{rspec} with the processed data.
#' @export
#' @examples \dontrun{
#' data(teal)
#' plot(teal, select = 10)
#' # Smooth data to remove noise
#' teal.sm <- procspec(teal, opt = 'smooth', span = 0.25)
#' plot(teal.sm, select = 10)
#' # Normalize to max of unity
#' teal.max <- procspec(teal, opt = c('max'), span = 0.25)
#' plot(teal.max, select = 10)}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @seealso \code{\link{loess.smooth}}
#' @references Cuthill, I., Bennett, A. T. D., Partridge, J. & Maier, E. 1999. Plumage reflectance and the objective assessment of avian sexual dichromatism. The American Naturalist, 153, 183-200.
#' @references Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J., eds. Bird Coloration. Volume 1 Mechanisms and measuremements. Harvard University Press, Cambridge, Massachusetts.

procspec <- function(rspecdata, opt = c('none', 'smooth', 'maximum', 'minimum', 
										 'bin', 'sum', 'center'), fixneg = c('none', 'addmin', 'zero'),
										 span = .25, bins = 20, method=c("loess", "spline"), spar=.65) {

opt <- match.arg(opt, several.ok = TRUE)
method <- match.arg(method)
fixneg <- match.arg(fixneg)

applied <- 'processing options applied:\n'

if (any(opt=='none')) {
  opt <- 'none' # remove other opt arguments (so they are not called further on, but still allowing for fixneg to work)
  applied <- 'No relevant processing option entered; returning raw values\n'
#  rspecdata <- rspecdata
#  class(rspecdata) <- c('rspec', 'data.frame')
#	return(rspecdata)
  }

wl_index <- which(names(rspecdata)=='wl')

if (length(wl_index > 0)){
  wl <- rspecdata[, wl_index]
  rspecdata <- as.data.frame(rspecdata[-wl_index])
    } else {
    warning('No wavelengths supplied; using arbitrary values')
    rspecdata <- as.data.frame(rspecdata)
    wl <- 1:nrow(rspecdata)
    }

nam <- names(rspecdata)

if (fixneg=='addmin'){
  adm = function(x){
    if (min(x) < 0){ x + abs(min(x))
    }else{
    x
    }
  }
  tempspc <- data.frame(sapply(1:ncol(rspecdata), function(z) adm(rspecdata[, z]) ) )
  names(tempspc) <- names(rspecdata)
  rspecdata <- round(tempspc,6)
  applied <- c(applied, 'Negative value correction: added min to all reflectance\n')
}

if (fixneg=='zero'){
  rspecdata[rspecdata < 0 ] <- 0
  applied <- c(applied, 'Negative value correction: added min to all reflectance\n')
}

# if (any(opt=='smooth')){
#   rspecdata <- sapply(names(rspecdata), function(z){loess.smooth(x = wl, 
#                   y = as.data.frame(rspecdata[, z]), span = span, degree = 2, 
#                   family = "gaussian", evaluation = length(wl))$y})
#   applied <- c(applied, paste('smoothing spectra with a span of',span,'\n'))
#   }

if (any(opt=='smooth')&method=='spline')
  rspecdata <- sapply(names(rspecdata), function(z){smooth.spline(x = wl, y = rspecdata[, z], 
           spar = spar)$y})

if (any(opt=='smooth')&method=='loess')
  rspecdata <- sapply(names(rspecdata), function(z){loess.smooth(x = wl, y = rspecdata[, z], 
           span = span, degree = 2, family = "gaussian", 
           evaluation = length(wl))$y})

if (any(opt=='minimum')){
  rspecdata <- sapply(1:ncol(rspecdata), function(z)rspecdata[, z] - min(rspecdata[, z]))
   applied <- c(applied, 'Scaling spectra to a minimum value of zero\n')
  }

if (any(opt=='maximum')){
  rspecdata <- sapply(1:ncol(rspecdata), function(z)rspecdata[, z] / max(rspecdata[, z]))
   applied <- c(applied, 'Scaling spectra to a maximum value of 1\n')
  }

if (any(opt=='sum')){
  rspecdata <- sapply(1:ncol(rspecdata), function(z)rspecdata[, z] / sum(rspecdata[, z]))
   applied <- c(applied, 'Scaling spectra to a total area of 1\n')
  }

if (any(opt=='center')){
  rspecdata <- sapply(1:ncol(rspecdata), function(z)rspecdata[, z] - mean(rspecdata[, z]))
   applied <- c(applied, 'Centering spectra to a mean of zero\n')
  }

# Calculate medians according to # of bins specified for use in PCA
# Method follows Cuthill et al. (1999)
if (any(opt=='bin')) {
  bw <- floor(length(wl)/bins)
  wl_bin <- seq(head(wl,1), tail(wl,1), by=bw)
  wl_ind <- match(wl_bin, wl)
  rspecdata <- sapply(1:(length(wl_ind)-1), function(z) 
                      apply(rspecdata[wl_ind[z]:(wl_ind[z] + bw),,drop=F], 2, median),
                      simplify=FALSE)

  rspecdata <- data.frame(matrix(unlist(rspecdata), nrow=bins, byrow=T))
  rspecdata <- as.data.frame(cbind(wl_bin[-length(wl_bin)], rspecdata))
  applied <- c(applied, paste('binned spectra to ',bw,'-nm intervals\n',sep=''))
  } else {
    rspecdata <- as.data.frame(cbind(wl, rspecdata))
    }

names(rspecdata) <- c('wl', nam)
class(rspecdata) <- c('rspec', 'data.frame')

applied <- c(applied, '\n')
cat(applied)

rspecdata

}
