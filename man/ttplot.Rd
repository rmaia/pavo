\name{ttplot}
\alias{ttplot}
\alias{ttpoints}
\alias{ttvol}
\title{Plot a Tetrahedral Color Space}
\usage{
  ttplot(tcsdata, size = 0.02, col = "black", new = FALSE,
    hspin = FALSE, vspin = FALSE, floor = TRUE,
    grid = TRUE, fill = TRUE)

  ttpoints(tcsdata, size = 0.02, col = "black")

  ttvol(tcsdata, col = "black", grid = T, fill = T)
}
\arguments{
  \item{tcsdata}{(required) a data frame, possibly a result
  from the \code{tcs} function, containing values for the
  'x', 'y' and 'z' coordinates as columns (labeled as
  such)}

  \item{size}{size of the points in the plot (defaults to
  0.02)}

  \item{col}{color of the points in the plot (defaults to
  black)}

  \item{new}{should a new 3D plot be called (defaults to
  \code{FALSE})?}

  \item{hspin}{if \code{TRUE}, the graphic will spin
  horizontally (around the 'z' axis)(defaults to
  \code{FALSE}).}

  \item{vspin}{if \code{TRUE}, the graphic will spin
  vertically (around the 'x' axis)(defaults to
  \code{FALSE}).}

  \item{floor}{if \code{TRUE}, a reference xy plane is
  plotted under the tetrahedron (defaults to \code{TRUE}).}

  \item{grid}{if \code{TRUE}, connects the polygon
  outlining the volume occupied by points (defaults to
  \code{TRUE}).}

  \item{fill}{if \code{TRUE}, fills the volume occupied by
  points (WARNING: transparency is not saved properly if
  exported using \code{rgl.postscript})(defaults to
  \code{TRUE}).}
}
\value{
  \code{ttplot} creates a 3D plot using functions of the
  package \code{rgl}, based on openGL capabilities. Plot is
  interactive and can be manipulated with the mouse (left
  button: rotate along 'z' axis; right button: rotate along
  'x' axis; third button: zoom). \code{ttvol} creates
  polygon based on points, determining the volume occupied
  by them in the colorspace. \code{ttpoints} adds points to
  the plot. Points are currently plotted only as spheres to
  maintain export capabilities.

  \code{ttplot} creates 3D points in a tetrahedral color
  space plot produced by \code{ttplot} using functions of
  the package \code{rgl}, based on openGL capabilities.

  \code{ttvol} creates a 3D convex hull within a
  \code{ttplot} object
}
\description{
  \code{ttplot} produces a 3D plot of a tetrahedral color
  space using OpenGL capabilities

  \code{ttpoints} plots points in a tetrahedral color space

  \code{ttvol} produces a 3D convex hull in tetrahedral
  color space
}
\examples{
\dontrun{
# For plotting
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual='avg.uv')
tcs.sicalis <- tcs(vis.sicalis)
ttplot(tcs.sicalis, size=0.005)
rgl.postscript('testplot.pdf',fmt='pdf')
rgl.snapshot('testplot.png')

# For adding points
patch <- rep(c('C','T','B'),7)
tcs.crown <- tcs.sicalis[patch=='C', ]
tcs.breast <- tcs.sicalis[patch=='B', ]
ttplot(tcs.crown, col='blue')
ttpoints(tcs.breast, col='red')

# For plotting convex hull
ttplot(tcs.sicalis, col='blue', size=.005)
ttvol(tcs.sicalis)}
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian
  plumage color in a tetrahedral color space: A
  phylogenetic analysis of new world buntings. The American
  Naturalist, 171(6), 755-776.

  Endler, J. A., & Mielke, P. (2005). Comparing entire
  colour patterns as birds see them. Biological Journal Of
  The Linnean Society, 86(4), 405-431.
}
\seealso{
  \code{\link{spheres3d}},\code{\link{rgl.postscript}},
  \code{\link{rgl.snapshot}},\code{\link{rgl.material}}
}

