#' Converts bewtween irradiance and photon (quantum) flux
#'
#' Some spectrometers will give illuminant values in units of irradiance (uWatt*cm^-2),
#' but physiological models require illuminants in units of photon (quantum) flux 
#' (umol*s^-1*m^-2). The functions \code{irrad2flux} and \code{flux2irrad} allows for
#' easy conversion of \code{rspec} objects between these units.
#'
#' @param rspecdata (required) a rspec object containing illuminant values
#' @return a converted \code{rspec} object
#' @export irrad2flux flux2irrad
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}

irrad2flux <- function(rspecdata){

if(!is.rspec(rspecdata))
  stop('not an object of class rspec')

  nam <- names(rspecdata)
  wl_index <- which(names(rspecdata)=='wl')
  wl <- rspecdata[,wl_index]
  #rspecdata <- rspecdata[,-wl_index]	
  K <- 0.01/(6.626*2.998*6.02308)
  
  res <- sapply(1:ncol(rspecdata), function(z) rspecdata[,z] * wl * K )
  res <- data.frame(res)
  names(res) <- nam
  class(res) <- c('rspec', 'data.frame')
  
  res[,'wl'] <- wl
  
  res
}


#' @rdname irrad2flux
#' @return a converted \code{rspec} object


flux2irrad <- function(rspecdata){

if(!is.rspec(rspecdata))
  stop('not an object of class rspec')

  nam <- names(rspecdata)
  wl_index <- which(names(rspecdata)=='wl')
  wl <- rspecdata[,wl_index]
  #rspecdata <- rspecdata[,-wl_index]	
  K <- 0.01/(6.626*2.998*6.02308)
  
  res <- sapply(1:ncol(rspecdata), function(z) rspecdata[,z] / (wl * K) )
  res <- data.frame(res)
  names(res) <- nam
  class(res) <- c('rspec', 'data.frame')
  
  res[,'wl'] <- wl
  
  res
}
