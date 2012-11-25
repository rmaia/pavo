\name{explorespec}
\alias{explorespec}
\title{Plot spectral curves}
\usage{
  explorespec(specdata, specreps = 1, lwd = 2,
    scale = c("free", "equal"))
}
\arguments{
  \item{specdata(required)}{a data frame, possibly an
  object of class \code{rspec} that has wavelength range in
  the first column, named 'wl', and spectral measurements
  in the remaining columns.}

  \item{specreps}{number of spectra to include in each
  graph (defaults to 1)}

  \item{lwd}{Width of the lines displayed on the plots
  (defaults to 2)}

  \item{scale}{defines how the y-axis should be scaled.
  \code{'free'}: panels can vary in the range of the
  y-axis; \code{'equal'}: all panels have the y-axis with
  the same range.}
}
\value{
  Spectral curve plots
}
\description{
  Plots one or multiple spectral curves in the same graph
  to rapidly compare groups of spectra.
}
\note{
  Number of plots presented per page depends on the number
  of curves per graph.
}
\author{
  Pierre-Paul Bitton \email{bittonp@uwindsor.ca}
}

