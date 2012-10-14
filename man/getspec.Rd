\name{getspec}
\alias{getspec}
\title{Import spectra files}
\usage{
  getspec(where, ext = "txt", lim = c(300, 700),
    decimal = ".", negative = c("zero", "min", "raw"),
    subdir = FALSE)
}
\arguments{
  \item{ext}{file extension to be searched for, without the
  "." (defaults to "txt").}

  \item{lim}{a vector with two numbers determining the
  wavelength limits to be considered (defaults to 300 and
  700).}

  \item{decimal}{character to be used to identify decimal
  plates (defaults to ".")}

  \item{negative}{how to handle negative reflectance.
  \code{zero}, transforms negative values by rounding them
  up to 0; \code{min} transforms by adding the lowest
  negative reflectance value for that spectrum to the
  reflectance at remaining wavelengths; \code{raw} does not
  transform and leaves negative values as is (not
  recommended). \code{zero} is recommended when negative
  values are a product ofnoise, whereas \code{min} is
  recommended when negative values are a product of bad
  reference calibration. (defaults to \code{zero})}

  \item{subdir}{should subdirectories within the
  \code{where} folder be included in the search? (defaults
  to \code{FALSE}.)}
}
\value{
  a data frame containing individual imported spectral
  files as columns. Reflectance values are interpolated to
  the nearest wavelength integer.
}
\description{
  Finds and imports spectra files from a folder. Currently
  tested and working for reflectance files generated in
  Ocean Optics SpectraSuite (USB2000, USB4000 and Jaz
  spectrometers), CRAIC software (after exporting) and
  Avantes (after exporting).
}
\examples{
\dontrun{
getspec('/examplespec', lim=c(400,900))
getspec('/examplespec', ext='ttt')}
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw
  K (eds) Bird coloration. Harvard University Press,
  Cambridge, pp 90–147.
}

