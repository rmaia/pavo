\name{as.rspec}
\alias{as.rspec}
\title{Convert data to an rspec object}
\usage{
  as.rspec(specs, whichwl = NULL)
}
\arguments{
  \item{specs}{(required) an \code{rspec} object containing
  spectra to process}

  \item{whichwl}{specifies which column contains
  wavelengths. If NULL (default), function searches for
  column titled "wl" or else returns arbitrary index
  values}
}
\value{
  an object of class \code{rspec} for use in further
  \code{pavo} functions
}
\description{
  Converts data frames or matrices containing spectral data
  to \code{rspec} object
}
\examples{
\dontrun{
#INCLUDE EXAMPLE}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}

