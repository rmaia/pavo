\name{ttplot}
\alias{ttplot}
\title{Plot a Tetrahedral Color Space}
\usage{
  ttplot(tcsres, size = 0.02, col = "black", new = T,
    hspin = T, vspin = F, floor = T)
}
\arguments{
  \item{tcres}{a data frame, possibly a result from the
  \code{tcs} function, containing values for the 'x', 'y'
  and 'z' coordinates as columns (labeled as such)}

  \item{size}{size of the points in the plot}

  \item{col}{color of the points in the plot}

  \item{new}{should a new 3D plot be called?}

  \item{hspin}{if \code{TRUE}, the grapihic will spin
  horizontally (around the 'z' axis)}

  \item{vspin}{if \code{TRUE}, the grapihic will spin
  vertically (around the 'x' axis)}

  \item{floor}{if \code{TRUE}, a reference xy plane is
  plotted under the tetrahedron}

  \item{grid}{if \code{TRUE}, connects the polygon
  outlining the volume occupied by points}

  \item{fill}{if \code{TRUE}, fills the volume occupied by
  points (WARNING: transparency is not saved properly if
  exported using \code{rgl.postscript})}
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
}
\description{
  Produces a 3D plot of a tetrahedral color space using
  OpenGL capabilities
}
\examples{
\dontrun{
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual='avg.uv')
tcs.sicalis <- tcs(vis.sicalis, by=rep(c('C','T','B'),7))
ttplot(tcs.sicalis$tcs, size=0.005)
ttvol(tcs.sicalis$tcs[grep('C$',row.names(tcs.sicalis$tcs)),],col='red')
ttvol(tcs.sicalis$tcs[grep('B$',row.names(tcs.sicalis$tcs)),],col='violet')
ttvol(tcs.sicalis$tcs[grep('T$',row.names(tcs.sicalis$tcs)),],col='orange')
rgl.postscript('testplot.pdf',fmt='pdf')
rgl.snapshot('testplot.png')}
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
  The Linnean Society, 86(4), 405–431.
}
\seealso{
  \code{\link{spheres3d}},\code{\link{rgl.postscript}},
  \code{\link{rgl.snapshot}},\code{\link{rgl.par3d}}
}

