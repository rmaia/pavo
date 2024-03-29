% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/voloverlap.R
\name{voloverlap}
\alias{voloverlap}
\title{Colour volume overlap}
\usage{
voloverlap(
  colsp1,
  colsp2,
  type = c("convex", "alpha"),
  avalue = "auto",
  plot = FALSE,
  interactive = FALSE,
  col = c("blue", "red", "darkgrey"),
  fill = FALSE,
  new = TRUE,
  nsamp = 1000,
  psize = 0.001,
  lwd = 1,
  ...
)
}
\arguments{
\item{colsp1, colsp2}{(required) data frame, possibly a result from the \code{\link[=colspace]{colspace()}}
function, containing
values for the 'x', 'y' (and possibly 'z') coordinates as columns (labeled as such)}

\item{type}{if "convex", the colour volume is plotted using a convex hull
and if "alpha", it is plotted using alphashapes.}

\item{avalue}{if \code{type = alpha}, the alpha parameter values for \code{colsp1} and
\code{colsp2} respectively to compute the alphashapes. Can be a numeric of length
one if the same value is used in both cases. \code{avalue = "auto"} (default)
finds and use the \eqn{\alpha^*}{alpha*} value as defined in Gruson (2020).}

\item{plot}{logical. Should the volumes and points be plotted? (defaults to \code{FALSE}).
This only works for tetrahedral colourspaces at the moment.}

\item{interactive}{logical. If \code{TRUE}, uses the rgl engine for interactive plotting;
if \code{FALSE} then a static plot is generated.}

\item{col}{a vector of length 3 with the colours for (in order) the first volume,
the second volume, and the overlap.}

\item{fill}{logical. should the two volumes be filled in the plot? (defaults to \code{FALSE})}

\item{new}{logical. Should a new plot window be called? If \code{FALSE}, volumes and their
overlap are plotted over the current plot (defaults to \code{TRUE}).}

\item{nsamp}{if \code{type = "alpha"}, the number of points to be sampled for the
Monte Carlo computation. Stoddard & Stevens(2011) use around 750,000 points,
but more or fewer might be required depending on the
degree of overlap.}

\item{psize}{if \code{type = "alpha"} and \code{plot = TRUE}, sets the size to plot the points
used in the Monte Carlo computation.}

\item{lwd}{if \code{plot = TRUE}, sets the line width for volume grids.}

\item{...}{additional arguments passed to the plot. See \code{\link[=vol]{vol()}}}
}
\value{
Calculates the overlap between the volumes defined by two set of points in
colourspace. The volume from the overlap is then given relative to:
\itemize{
\item \code{vsmallest} the volume of the overlap divided by the smallest of that defined
by the the two input sets of colour points. Thus, if one of the volumes is entirely
contained within the other, this overlap will be \code{vsmallest = 1}.
\item \code{vboth} the volume of the overlap divided by the combined volume of both
input sets of colour points.
If \code{type = "alpha"}, If used, the output will be different:
\item \verb{s_in1, s_in2} the number of sampled points that fall within each of the volumes
individually.
\item \code{s_inboth} the number of sampled points that fall within both volumes.
\item \code{s_ineither} the number of points that fall within either of the volumes.
\item \code{psmallest} the proportion of points that fall within both volumes divided by the
number of points that fall within the smallest volume.
\item \code{pboth} the proportion of points that fall within both volumes divided by the total
number of points that fall within both volumes.
}
}
\description{
Calculates the overlap between the volumes defined by two sets of points in cartesian
space.
}
\note{
Stoddard & Stevens (2011) originally obtained the volume overlap through Monte Carlo
simulations of points within the range of the volumes, and obtaining the frequency of
simulated values that fall inside the volumes defined by both sets of colour points.

Stoddard & Stevens (2011) also return the value of the overlap relative to one of
the volumes (in that case, the host species). However, for other applications
this value may not be what one expects to obtain if (1) the two
volumes differ considerably in size, or (2) one of the volumes is entirely contained
within the other. For this reason, we also report the volume relative to the union of
the two input volumes, which may be more adequate in most cases.
}
\examples{
data(sicalis)
tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")

# Convex hull volume
voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "convex")
voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex", plot = TRUE)
voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex", plot = TRUE, col = seq_len(3))

# Alpha-shape volume
if (require("alphashape3d")) {
  voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "alpha", avalue = 1)
}
}
\references{
Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color
in a tetrahedral color space: A phylogenetic analysis of new world buntings. The
American Naturalist, 171(6), 755-776.

Stoddard, M. C., & Stevens, M. (2011). Avian vision and the evolution of
egg color mimicry in the common cuckoo. Evolution, 65(7), 2004-2013.

Maia, R., White, T. E., (2018) Comparing colors using visual models.
Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}

Gruson H. (2020). Estimation of colour volumes as concave hypervolumes using
\eqn{\alpha}{alpha}-shapes. Methods in Ecology and Evolution, 11(8), 955-963
\doi{10.1111/2041-210X.13398}
}
\author{
Rafael Maia \email{rm72@zips.uakron.edu}

Hugo Gruson \email{hugo.gruson+R@normalesup.org}
}
