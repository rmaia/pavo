\name{peakshape}
\alias{peakshape}
\title{Peak shape descriptors}
\usage{
  peakshape(rspecdata, select = NULL, lim = c(300, 700),
    plot = TRUE, ask = FALSE, ...)
}
\arguments{
  \item{rspecdata}{(required) a data frame, possibly an
  object of class \code{rspec}, with a column with
  wavelength data, named 'wl', and the remaining column
  containing spectra to process.}

  \item{select}{specification of which spectra to plot. Can
  be a numeric vector or factor (e.g., \code{sex=='male'})}

  \item{lim}{a vector specifying the wavelength range to
  analyze}

  \item{plot}{logical. Should plots indicating calculated
  parameters be returned? (Defaults to \code{TRUE})}

  \item{ask}{logical, specifies whether user input needed
  to plot multiple plots when number of spectra to analyze
  is greater than 1 (defaults to \code{FALSE})}

  \item{...}{additional arguments to be passed to plot}
}
\value{
  a data frame containing column names (id); peak height
  (max value, B3), location (hue, H1) and full width at
  half maximum (FWHM), as well as half widths on left
  (HWHM.l) and right side of peak (HWHM.r). Incl.min column
  indicates whether user-defined bounds incorporate the
  actual minima of the spectra. Function will return a
  warning if not.
}
\description{
  Calculates height, location and width of peak at the
  reflectance midpoint (FWHM). Note: bounds should be set
  wide enough to incorporate all minima in spectra.
  Smoothing spectra using \code{\link{procspec}} is also
  recommended.
}
\examples{
\dontrun{
data(teal)
peakshape(teal, select = 3)
peakshape(teal, select = 10)
# Use wavelength bounds to narrow in on peak of interest
peakshape(teal, select = 10, lim=c(400, 550))}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}, Rafael Maia
  \email{rm72@zips.uakron.edu}
}
\seealso{
  \code{\link{procspec}}
}

