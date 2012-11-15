\name{plotsmooth}
\alias{plotsmooth}
\title{Plot loess smoothed curves}
\usage{
  plotsmooth(all.specs, minsmooth = 0.05, maxsmooth = 0.2,
    curves = 5, specnum = 0)
}
\arguments{
  \item{all.specs(required)}{A data frame of format
  \code{rspec}}

  \item{minsmooth}{The minimum f value of the loess
  function to visualize (defaults to 0.02).}

  \item{maxsmooth}{The maximum f value of the loess
  function to visualize (defaults to 0.20).}

  \item{gap}{The number of curves to display on the same
  plot (defaults to 5).}

  \item{specnum}{The number of spectral curves, from the
  data frame, to visualize (defaults to ALL).}
}
\value{
  Series of plots with curves processed with varying level
  of loess smoothing.
}
\description{
  Plots curves with various levels of loess smoothing to
  help determine what smoothing parameters are best for the
  data.
}
\author{
  Pierre-Paul Bitton \email{bittonp@uwindsor.ca}
}

