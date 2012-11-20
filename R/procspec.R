#' Process spectra
#'
#' Applies normalization and/or smoothing to spectra for further analysis or plotting
#'
#' @param specs (required) an \code{rspec} object containing spectra to process
#' @param select specification of which spectra to plot. Can be a numeric vector or 
#' factor (e.g., \code{sex=='male'})
#' @param opt what type of processing options to apply. User can select multiple options
#'            by providing a vector. Possibilites are:
#' \itemize{
#'	\item \code{"none"} does not perform any processing (default).
#'	\item \code{"smooth"} applies LOESS smoothing to each spectrum using 
#'                      \code{loess.smooth}.
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
#' @export
#' @examples \dontrun{
#' #INCLUDE EXAMPLE}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @seealso \code{\link{loess.smooth}}
#' @references Cuthill, I., Bennett, A. T. D., Partridge, J. & Maier, E. 1999. Plumage reflectance and the objective assessment of avian sexual dichromatism. The American Naturalist, 153, 183–200.
#' @references Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J., eds. Bird Coloration. Volume 1 Mechanisms and measuremements. Harvard University Press, Cambridge, Massachusetts.

# TODO
# * inputting only one spec gave output of wavelengths only-how do we want to handle it
#   when users don't supply wavelengths?
# * supplied data frame must be an rspec object--put relevant warnings in place if not

procspec <- function(specs, opt = c('none', 'smooth', 'maximum', 'minimum', 'stretch', 
										 'bin', 'sum', 'center'), 
										 fixneg = c('none', 'addmin', 'zero'),
										 span = .25, bins = 20) {

opt <- match.arg(opt, several.ok = TRUE)

fixneg <- match.arg(fixneg)

applied <- 'processing options applied:\n'

if (any(opt=='none')) {
  opt <- 'none' # remove other opt arguments (so they are not called further on, but still allowing for fixneg to work)
  applied <- 'No relevant processing option entered; returning raw values\n'
#  specs <- specs
#  class(specs) <- c('rspec', 'data.frame')
#	return(specs)
  }

wl_index <- which(names(specs)=='wl')

if (length(wl_index > 0)){
  wl <- specs[, wl_index]
  specs <- as.data.frame(specs[, -wl_index])
    } else {
    warning('No wavelengths supplied; using arbitrary values')
    specs <- as.data.frame(specs)
    wl <- 1:nrow(specs)
    }

if (fixneg=='addmin'){
  adm = function(x){
    if (min(x) < 0){ x + abs(min(x))
    }else{
    x
    }
  }
  tempspc <- data.frame(sapply(1:ncol(specs), function(z) adm(specs[, z]) ) )
  names(tempspc) <- names(specs)
  specs <- round(tempspc,6)
  applied <- c(applied, 'Negative value correction: added min to all reflectance\n')
}

if (fixneg=='zero'){
  specs[specs < 0 ] <- 0
  applied <- c(applied, 'Negative value correction: added min to all reflectance\n')
}

if (any(opt=='smooth')){
  specs <- sapply(names(specs), function(z){loess.smooth(x = wl, 
                  y = as.data.frame(specs[, z]), span = span, degree = 2, 
                  family = "gaussian", evaluation = length(wl))$y})
  applied <- c(applied, paste('smoothing spectra with a span of',span,'\n'))
  }

# if (any(opt=='smooth')&method=='spline')
  # specs <- sapply(names(specs), function(z){smooth.spline(x = wl, y = specs[, z], 
           # spar = spar)$y})

# if (any(opt=='smooth')&method=='loess')
  # specs <- sapply(names(specs), function(z){loess.smooth(x = wl, y = specs[, z], 
           # span = span, degree = 2, family = "gaussian", 
           # evaluation = length(wl))$y})

if (any(opt=='minimum')){
  specs <- sapply(1:ncol(specs), function(z)specs[, z] - min(specs[, z]))
   applied <- c(applied, 'Scaling spectra to a minimum value of zero\n')
  }

if (any(opt=='maximum')){
  specs <- sapply(1:ncol(specs), function(z)specs[, z] / max(specs[, z]))
   applied <- c(applied, 'Scaling spectra to a maximum value of 1\n')
  }

if (any(opt=='sum')){
  specs <- sapply(1:ncol(specs), function(z)specs[, z] / sum(specs[, z]))
   applied <- c(applied, 'Scaling spectra to a total area of 1\n')
  }

if (any(opt=='center')){
  specs <- sapply(1:ncol(specs), function(z)specs[, z] - mean(specs[, z]))
   applied <- c(applied, 'Centering spectra to a mean of zero\n')
  }

# Calculate medians according to # of bins specified for use in PCA
# Method follows Cuthill et al. (1999)
if (any(opt=='bin')) {
  bw <- floor(length(wl)/bins)
  wl_bin <- seq(head(wl,1), tail(wl,1), by=bw)
  wl_ind <- match(wl_bin, wl)
  specs <- sapply(1:(length(wl_ind)-1), function(z) 
                  apply(specs[wl_ind[z]:(wl_ind[z]+bw), ], 2, median))

  specs <- as.data.frame(cbind(wl_bin[-length(wl_bin)], t(specs)))
   applied <- c(applied, paste('binned spectra to ',bw,'-nm intervals\n',sep=''))
  }else {
    specs <- as.data.frame(cbind(wl, specs))
    }
	
class(specs) <- c('rspec', 'data.frame')

applied <- c(applied, '\n')
cat(applied)

specs

}
