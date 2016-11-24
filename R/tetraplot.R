#' Plot a Static Tetrahedral Colorspace
#'
#' \code{tetraplot} produces a static 3D plot of a tetrahedral 
#'  colorspace. Accessed via the function \code{\link{plot.colspace}}.
#' 
#' @import scatterplot3d
#' 
#' @param tcsdata (required) a data frame, possibly a result from the \code{colspace} 
#'   function, containing values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param view orientation of the tetrahedron in degrees (defaults to 70)
#' @param vertexsize size of the points at the vertices
#' @param achrosize size of the point in the achromatic center
#' @param achrocol color of the point in the achromatic center
#' @param linwd line width for the edges of the tetrahedron
#' @param lincol line color for the edges of the tetrahedron
#' 
#' @return \code{tetraplot} creates a 3D plot using functions of the package \code{scatterplot3d}.
#'
#' @examples \dontrun{
#' 
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'avg.uv')
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#' plot(tcs.sicalis)
#' 
#' }
#' 
#' @seealso \code{\link[rgl]{spheres3d}},\code{\link[rgl]{rgl.postscript}}, 
#' \code{\link[rgl]{rgl.snapshot}},\code{\link[rgl]{rgl.material}} 
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#' 
#' @keywords internal
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage 
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings. 
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns 
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

tetraplot<- function(tcsdata, vertexsize = 0.8, achro = TRUE, achrosize = 0.8, 
                     achrocol = 'grey', linwd = 1, lincol = 'darkgrey', view = 70, ...) {
  
    # Set defaults
    arg <- list(...)
    
    if(is.null(arg$col))
      arg$col <- 'black'
    if(is.null(arg$cex))
      arg$cex <- 0.9
    if(is.null(arg$pch))
      arg$pch <- 19
  
    # Empty plot
    P <- scatterplot3d(0, 0, 0, box = TRUE, 
                                  xlim = c(-1.22, 0.612), ylim = c(-0.35, 0.707), 
                                  zlim = c(-0.25, 0.75), axis = F, grid = F, angle = view, 
                                  scale.y = 0.45, mar = c(1, 1, 1, 1))
    
    # Vertices
    u <- P$xyz.convert(0, 0, 0.75)
    s <- P$xyz.convert((-1 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25)
    m <- P$xyz.convert(0, (1/sqrt(2)), -0.25)
    l <- P$xyz.convert((0.5 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25)
    no.u <- P$xyz.convert(0, 0, -0.25)
    
    # Draw it up
    segments(u$x, u$y, l$x, l$y, col = lincol, lwd = linwd)
    segments(u$x, u$y, m$x, m$y, col = lincol, lwd = linwd)
    segments(u$x, u$y, s$x, s$y, col = lincol, lwd = linwd)
    segments(s$x, s$y, l$x, l$y, col = lincol, lwd = linwd)
    segments(m$x, m$y, s$x, s$y, col = lincol, lwd = linwd)
    segments(l$x, l$y, m$x, m$y, col = lincol, lwd = linwd)
  
  # Origin
    if(achro == TRUE)
      P$points3d(0, 0, 0, col = achrocol, cex = achrosize, pch = 15)
  
  # Data
    arg$x <- tcsdata$x
    arg$y <- tcsdata$y
    arg$z <- tcsdata$z
    
    do.call(P$points3d, arg)
  
  # Verticy points
  points(x = u$x, y = u$y, col = 'black', bg = 'darkorchid1', pch = 21, cex = vertexsize)
  points(x = s$x, y = s$y, col = 'black', bg = 'cornflowerblue', pch = 21, cex = vertexsize)
  points(x = m$x, y = m$y, col = 'black', bg = 'mediumseagreen', pch = 21, cex = vertexsize)
  points(x = l$x, y = l$y, col = 'black', bg = 'firebrick1', pch = 21, cex = vertexsize)
  
  # Save plot info 
   .PlotTetraEnv <<- new.env()
   assign("last_plot.tetra", P, envir = .PlotTetraEnv)
   
}
