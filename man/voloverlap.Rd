\name{voloverlap}
\alias{voloverlap}
\title{Color volume overlap}
\usage{
  voloverlap(tcsres1, tcsres2, plot = FALSE,
    col = c("blue", "red", "darkgrey"), new = TRUE)
}
\arguments{
  \item{tcsres1,tcsres2}{(required) data frame, possibly a
  result from the \code{tcs} function, containing values
  for the 'x', 'y' and 'z' coordinates as columns (labeled
  as such)}

  \item{plot}{logical. Should the volumes and points be
  plotted? (defaults to \code{FALSE})}

  \item{col}{a vector of length 3 with the colors for (in
  order) the first volume, the second volume, and the
  overlap.}

  \item{new}{logical. Should a new plot window be called?
  If \code{FALSE}, volumes and their overlap are plotted
  over the current plot (defaults to \code{TRUE}).}
}
\value{
  Calculates the overlap between the volumes defined by two
  set of points in colorspace. The volume from the overlap
  is then given relative to: \itemize{ \item
  \code{vsmallest} the volume of the overlap divided by the
  smallest of that defined by the the two input sets of
  color points. Thus, if one of the volumes is entirely
  contained within the other, this overlap will be
  \code{vsmallest = 1}.  \item \code{vboth} the volume of
  the overlap divided by the combined volume of both input
  sets of color points. }
}
\description{
  Calculates the overlap between the volumes defined by two
  sets of points in cartesian space
}
\note{
  Stoddard & Stevens (2011) originally obtained the volume
  overlap through Monte Carlo simulations of points within
  the range of the volumes, and obtaining the frequency of
  simulated values that fall inside the volumes defined by
  both sets of color points.

  Here we present an exact solution based on finding common
  vertices to both volumes and calculating its volume.

  Stoddard & Stevens (2011) also return the value of the
  overlap relative to one of the volumes (in that case, the
  host species). However, for other applications this value
  may not be what one expects to obtain if (1) the two
  volumes differ considerably in size, or (2) one of the
  volumes is entirely contained within the other. For this
  reason, we also report the volume relative to the union
  of the two input volumes, which may be more adequate in
  most cases.
}
\examples{
\dontrun{
data(sicalis)
tcs.sicalis.C <- tcs(vismodel(sicalis[c(1,grep('\\\\.C',names(sicalis)))]))
tcs.sicalis.T <- tcs(vismodel(sicalis[c(1,grep('\\\\.T',names(sicalis)))]))
tcs.sicalis.B <- tcs(vismodel(sicalis[c(1,grep('\\\\.B',names(sicalis)))]))
voloverlap(tcs.sicalis.T,tcs.sicalis.B)
voloverlap(tcs.sicalis.T,tcs.sicalis.C, plot=T)
voloverlap(tcs.sicalis.T,tcs.sicalis.C, plot=T, col=1:3) }
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

