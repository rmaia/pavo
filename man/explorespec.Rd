\name{explorespec}
\alias{explorespec}
\title{Plot spectral curves}
\usage{
  explorespec(rspecdata, by = 1,
    scale = c("free", "equal"), ...)
}
\arguments{
  \item{rspecdata}{(required) a data frame, possibly an
  object of class \code{rspec} that has wavelength range in
  the first column, named 'wl', and spectral measurements
  in the remaining columns.}

  \item{by}{number of spectra to include in each graph
  (defaults to 1)}

  \item{scale}{defines how the y-axis should be scaled.
  \code{'free'}: panels can vary in the range of the
  y-axis; \code{'equal'}: all panels have the y-axis with
  the same range.}

  \item{...}{additional parameters to be passed to plot}
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
  of graphs produced.
}
\examples{
\dontrun{
data(sicalis)
explorespec(sicalis,3)}
}
\author{
  Pierre-Paul Bitton \email{bittonp@uwindsor.ca}
}

