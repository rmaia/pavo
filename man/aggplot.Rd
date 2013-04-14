\name{aggplot}
\alias{aggplot}
\title{Plot aggregated reflectance spectra}
\usage{
  aggplot(rspecdata, by = NULL, FUN.center = mean,
    FUN.error = sd, lcol = NULL, shadecol = NULL,
    alpha = 0.2, ...)
}
\arguments{
  \item{rspecdata}{(required) data frame containing the
  spectra to be manipulated and plotted.}

  \item{by}{(required) either a single value specifying the
  range of spectra within the data frame to be combined
  (for example, \code{by} = 3 indicates the function will
  be applied to groups of 3 consecutive columns in the
  spectra data frame) or a vector containing
  identifications for the columns in the spectra data frame
  (in which case the function will be applied to each group
  of spectra sharing the same identification).}

  \item{FUN.center}{the function to be applied to the
  groups of spectra, calculating a measure of central
  tendency (defaults to \code{mean}).}

  \item{FUN.error}{the function to be applied to the groups
  of spectra, calculating a measure of variation (defaults
  to \code{sd}).}

  \item{lcol}{color of plotted lines indicating central
  tendency.}

  \item{shadecol}{color of shaded areas indicating variance
  measure.}

  \item{alpha}{transparency of the shaded areas.}

  \item{...}{additional graphical parameters to be passed
  to plot.}
}
\value{
  Plot containing the lines and shaded areas of the groups
  of spectra.
}
\description{
  Combines and plots spectra (by taking the average and the
  standard deviation, for example) according to an index or
  a vector of identities.
}
\examples{
\dontrun{
data(sicalis)
bysic <- gsub("^ind[0-9].",'',names(sicalis)[-1])
aggplot(sicalis,bysic)
aggplot(sicalis,bysic, shade=spec2rgb(sicalis),lcol=1)
aggplot(sicalis,bysic,lcol=1, FUN.error=function(x)sd(x)/sqrt(length(x))) }
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}, Chad Eliason
  \email{cme16@zips.uakron.edu}
}
\references{
  Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw
  K (eds) Bird coloration. Harvard University Press,
  Cambridge, pp 90-147.
}

