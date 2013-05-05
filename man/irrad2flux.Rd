\name{irrad2flux}
\alias{flux2irrad}
\alias{irrad2flux}
\title{Converts bewtween irradiance and photon (quantum) flux}
\usage{
  irrad2flux(rspecdata)

  flux2irrad(rspecdata)
}
\arguments{
  \item{rspecdata}{(required) a rspec object containing
  illuminant values}
}
\value{
  a converted \code{rspec} object

  a converted \code{rspec} object
}
\description{
  Some spectrometers will give illuminant values in units
  of irradiance (uWatt*cm^-2), but physiological models
  require illuminants in units of photon (quantum) flux
  (umol*s^-1*m^-2). The functions \code{irrad2flux} and
  \code{flux2irrad} allows for easy conversion of
  \code{rspec} objects between these units.
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}

