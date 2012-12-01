\name{aggspec}
\alias{aggspec}
\title{Aggregate reflectance spectra}
\usage{
  aggspec(rspecdata, by, FUN = mean)
}
\arguments{
  \item{rspecdata}{(required) data frame, possibly of class
  \code{rspec} containing the spectra to be manipulated. If
  it contains a wavelength column containing "wl", that
  column will be ignored.}

  \item{by}{(required) either a single value specifying the
  range of spectra within the data frame to be combined
  (for example, \code{by} = 3 indicates the function will
  be applied to groups of 3 consecutive columns in the
  spectra data frame) or a vector containing
  identifications for the columns in the spectra data frame
  (in which case the function will be applied to each group
  of spectra sharing the same identification).}

  \item{FUN}{the function to be applied to the groups of
  spectra. (defaults to \code{\link{mean}})}
}
\value{
  A data frame of class \code{rspec} containing the spectra
  after applying the aggregating function.
}
\description{
  Combines spectra (by taking the average, for example)
  according to an index or a vector of identities.
}
\examples{
\dontrun{
specs <- getspec('/examplespec')
specs <- data.frame(cbind(specs, specs[,-1],specs[,-1],specs[,-1]))
aggspec(specs,by=2)
aggspec(specs,by=c('a','b','a','b','a','b','a','b')) }
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}
\references{
  Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw
  K (eds) Bird coloration. Harvard University Press,
  Cambridge, pp 90-147.
}

