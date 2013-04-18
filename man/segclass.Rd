\name{segclass}
\alias{segclass}
\title{Segment classification}
\usage{
  segclass(rspecdata, range = c(300, 700))
}
\arguments{
  \item{rspecdata}{(required) a data frame, such as objects
  of class \code{rspec}, with a column containing
  wavelength range, named 'wl' and spectra data in
  remaining columns.}

  \item{range}{vector of length 2 indicating the lower and
  upper wavelength bounds used to calculate segments
  (defaults to 300nm to 700nm).}
}
\value{
  A data frame with LM and MS segment classification
  scores.
}
\description{
  Calculates segment classification measures as defined in
  Endler (1990)
}
\examples{
\dontrun{
data(sicalis)
segclass(sicalis) }
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

