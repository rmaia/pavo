\name{voloverlap2}
\alias{voloverlap2}
\title{Color volume overlap}
\usage{
  voloverlap2(tcsres1, tcsres2, plot = FALSE)
}
\arguments{
  \item{tcsres1}{(required) data frame, possibly a result
  from the \code{tcs} function, containing values for the
  'x', 'y' and 'z' coordinates as columns (labeled as
  such)}

  \item{tcsres2}{(required) data frame, possibly a result
  from the \code{tcs} function, containing values for the
  'x', 'y' and 'z' coordinates as columns (labeled as
  such)}

  \item{nsamp}{number of simulated points}

  \item{plot}{Should the volumes and points be plotted?
  (defaults to \code{FALSE}.)}

  \item{size}{Size of the spheres in the 3D volume plot (if
  called)}
}
\value{
  Calculates the overlap between the volumes defined by two
  set of points in colorspace. This is done by simulating
  points from a uniform distribution defined by the
  combined values of the points, and obtaining the
  frequency of simulated values that fall inside the
  volumes defeined by both sets of color points. This
  frequency is then compared to (1) the frequency of values
  that fall within the smallest volume (Stoddard & Stevens
  2011), and (2) the combined volume of both sets of color
  points.
}
\description{
  calculates the overlap between the volumes defined by two
  sets of points in cartesian space
}
\note{
  Stoddard & Stevens (2011) calculate the volume overlap
  relative to one of the volumes compared (i.e. how many of
  the simulated points that fall in volume 1 also fall in
  volume 2), and we return this value (which is always
  relative to the smallest volume). However, this value may
  not be what one expects to obtain if (1) the two volumes
  differ considerably in size, or (2) one of the volumes is
  entirely contained within the other. For this reason, we
  also report p(both)/(p(vol1)+p(vol2)), which may be more
  adequate in those cases.

  The simulation process requires the calculation of many
  convex hulls, and therefore may be computationally
  intensive and take considerably long if many points are
  simulated. Given that depending on the shape of the color
  volumes many of the points simulated may fall outside
  either volumes, make sure to check the output to see if a
  decent sample size is falling withing the volumes and
  being used in calculations.
}
\examples{
\dontrun{
data(sicalis)
tcs.sicalis.C <- tcs(vismodel(sicalis[c(1,grep('\\\\.C',names(sicalis)))]))
tcs.sicalis.T <- tcs(vismodel(sicalis[c(1,grep('\\\\.T',names(sicalis)))]))
tcs.sicalis.B <- tcs(vismodel(sicalis[c(1,grep('\\\\.B',names(sicalis)))]))
voloverlap(tcs.sicalis.T,tcs.sicalis.B, nsamp=5000)
voloverlap(tcs.sicalis.T,tcs.sicalis.C, plot=T) }
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian
  plumage color in a tetrahedral color space: A
  phylogenetic analysis of new world buntings. The American
  Naturalist, 171(6), 755-776.

  Stoddard, M. C., & Stevens, M. (2011). Avian vision and
  the evolution of egg color mimicry in the common cuckoo.
  Evolution, 65(7), 2004-2013.
}

