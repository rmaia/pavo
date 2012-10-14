\name{getspec}
\alias{getspec}
\title{Import spectra files}
\usage{
  getspec(where, ext = "txt", lim = c(300, 700),
    decimal = ".", subdir = F)
}
\arguments{
  \item{ext}{file extension to be searched for, without the
  "." (defaults to "txt").}

  \item{lim}{a vector with two numbers determining the
  wavelength limits to be considered (defaults to 300 and
  700).}

  \item{decimal}{character to be used to identify decimal
  plates (defaults to ".")}

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
\dontrun{}
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw
  K (eds) Bird coloration. Harvard University Press,
  Cambridge, pp 90–147
}

