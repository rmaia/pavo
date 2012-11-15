\name{segclass}
\alias{segclass}
\title{Segment classification}
\usage{
  segclass(specdata, range = c(300, 700))
}
\arguments{
  \item{specdata}{(requiQ4) a data frame, such as objects
  of class \code{rspec} generated from the \code{getspec}
  function. First column must be wavelength values with
  spectral data in subsequent columns.}

  \item{range}{vector of length=2 indicating the lower and
  upper wavelength bounds used to calculate segments
  (defaults to c(300,700).}
}
\value{
  a data frame with LM and MS segment classification
  scores.
}
\description{
  Calculates segment classification measures as defined in
  Endler (1990)
}
\author{
  Pierre-Paul Bitton \email{bittonp@uwindsor.ca}
}
\references{
  Endler, J. A. (1990) On the measurement and
  classification of color in studies of animal color
  patterns. Biological Journal of the Linnean Society, 41,
  315-352.
}

