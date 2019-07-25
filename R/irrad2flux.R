#' Converts between irradiance and photon (quantum) flux
#'
#' Some spectrometers will give illuminant values in units of irradiance
#' (\eqn{\mu Watt.cm^{-2}}), but physiological models require illuminants in units of
#' photon (quantum) flux (\eqn{\mu mol.s^{-1}.m^{-2}}). The functions `irrad2flux`
#' and `flux2irrad` allows for easy conversion of `rspec` objects
#' between these units.
#'
#' @param rspecdata (required) a rspec object containing illuminant values.
#' @return a converted `rspec` object.
#' @export irrad2flux flux2irrad
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}

irrad2flux <- function(rspecdata) {

  wl <- isolate_wl(rspecdata, keep = "wl")
  y  <- isolate_wl(rspecdata, keep = "spec")

  K <- 0.01 / (6.626068 * 2.99792458 * 6.02214076)

  y <- y * wl * K

  rspecdata <- cbind(wl, y)

  class(rspecdata) <- c("rspec", "data.frame")

  return(rspecdata)
}


#' @rdname irrad2flux

flux2irrad <- function(rspecdata) {

  wl <- isolate_wl(rspecdata, keep = "wl")
  y  <- isolate_wl(rspecdata, keep = "spec")

  K <- 0.01 / (6.626068 * 2.99792458 * 6.02214076)

  y <- y / (wl * K)

  rspecdata <- cbind(wl, y)

  class(rspecdata) <- c("rspec", "data.frame")

  return(rspecdata)
}
