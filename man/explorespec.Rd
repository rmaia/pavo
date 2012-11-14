\name{explorespec}
\alias{explorespec}
\title{Plot spectral curves}
\usage{
  explorespec(specdata, specreps = 1, lwd = 2)
}
\arguments{
  \item{specdata(required)}{a data frame, possibly an
  object of class \code{rspec} that has wavelength range in
  the first column and spectral measurements in the
  remaining columns.}

  \item{specreps}{number of spectra to include in each
  graph (defaults to 1)}
}
\value{
  Spectral curve plots
}
\description{
  Plots multiple spectral curves in the same graph to
  rapidly compare groups of spectra.
}
\note{
  Number of plots presented per page depends on the number
  of curves per graph.
}
\author{
  Pierre-Paul Bitton \email{bittonp@uwindsor.ca}
}

