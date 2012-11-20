\name{peakshape}
\alias{peakshape}
\title{Peak shape descriptors}
\usage{
  peakshape(specs, select = NULL, bounds = c(300, 700),
    plot = T, ...)
}
\arguments{
  \item{specs}{(required) an \code{rspec} object containing
  spectra to process}

  \item{select}{specification of which spectra to plot. Can
  be a numeric vector or factor (e.g., \code{sex=='male'})}

  \item{bounds}{a vector specifying the wavelength range to
  analyze}
}
\value{
  a data frame containing peak height (max value), location
  (hue) and full width at half maximum, as well as half
  widths on left and right side of peak. Status column
  indicates whether user-defined bounds incorporate the
  actual minima of the spectra. Function will return a
  warning if not.
}
\description{
  Calculates height, location and width of peak at the
  reflectance midpoint (FWHM). Note: bounds should be set
  wide enough to incorporate all minima in spectra.
  Smoothing spectra using \code{procspec} is also
  recommended.
}
\examples{
\dontrun{
data(sicalis)
sicalis.sm <- procspec(sicalis, opt='smooth', span=.25)
FWHM(sicalis.sm, select=2:5, bounds=c(300, 550))}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}, Rafael Maia
  \email{rm72@zips.uakron.edu}
}

