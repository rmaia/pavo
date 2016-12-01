#' Plot a static tetrahedral colorspace
#'
#' Produces a static 3D tetrahedral plot.
#' 
#' @import scatterplot3d
#' 
#' @param tcsdata (required) a data frame, possibly a result from the \code{colspace} 
#'   function, containing values for the 'x', 'y' and 'z' coordinates as columns (labeled as such).
#' @param view orientation of the tetrahedron in degrees (defaults to 70).
#' @param scale.y numeric. Perspective scaling of the y axis (defaults to 0.45).
#' @param axis logical. Draw X, Y and Z axis (defaults to FALSE).
#' @param grid logical. Draw grid (defaults to FALSE).
#' @param vertexsize size of the points at the vertices (defaults to 0.8).
#' @param achrosize size of the point in the achromatic center (defaults to 0.8).
#' @param achrocol color of the point in the achromatic center (defaults to 'grey').
#' @param out.lwd,out.lcol graphical parameters for the tetrahedral outline.
#' @param xlim,ylim,zlim axis limits.
#' @param margin vector of four numbers specifying drawing margins (defaults to c(1, 1, 1, 1)).
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
                     achrocol = 'grey', out.lwd = 1, out.lcol = 'darkgrey', 
                     view = 70, scale.y = 1, axis = FALSE, grid = FALSE,
                     xlim = c(-1.22, 0.612), ylim = c(-0.35, 0.707), 
                     zlim = c(-0.25, 0.75), 
                     margin = c(1, 1, 1, 1), ...) {
    
    if(view >= 360)
      view <- view - 360
    
    # Set defaults
    arg <- list(...)
    
    if(is.null(arg$col))
      arg$col <- 'black'
    if(is.null(arg$cex))
      arg$cex <- 0.9
    if(is.null(arg$pch))
      arg$pch <- 19
  
    # Empty plot
    P <- scatterplot3d(x=mean(xlim), y=mean(ylim), z=mean(zlim), box = FALSE, 
                                  xlim = xlim, ylim = ylim, zlim = zlim,
                                  axis = axis, grid = grid, angle = view, 
                                  scale.y = scale.y, mar = margin, pch = '')
    
    # Vertices
    u <- P$xyz.convert(0, 0, 0.75)
    s <- P$xyz.convert((-1 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25)
    m <- P$xyz.convert(0, (1/sqrt(2)), -0.25)
    l <- P$xyz.convert((0.5 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25)
    no.u <- P$xyz.convert(0, 0, -0.25)
    
    # Draw it up
    segments(u$x, u$y, m$x, m$y, col = out.lcol, lwd = out.lwd) #um
    segments(m$x, m$y, s$x, s$y, col = out.lcol, lwd = out.lwd) #sm
      
    if(view >= 80)
      segments(u$x, u$y, l$x, l$y, col = out.lcol, lwd = out.lwd) #ul

    if(view >= 180)
      segments(s$x, s$y, l$x, l$y, col = out.lcol, lwd = out.lwd) #sl      
    
    if(view < 80)
      segments(u$x, u$y, s$x, s$y, col = out.lcol, lwd = out.lwd) #us
 
    if(view < 180)	
    segments(l$x, l$y, m$x, m$y, col = out.lcol, lwd = out.lwd) #ml
    
  # Origin
    if(isTRUE(achro))
      P$points3d(0, 0, 0, col = achrocol, cex = achrosize, pch = 15)
  
  # Data
    arg$x <- tcsdata$x
    arg$y <- tcsdata$y
    arg$z <- tcsdata$z
    
    do.call(P$points3d, arg)
    
  # Draw vertices in front of points
    if(view < 80)
      segments(u$x, u$y, l$x, l$y, col = out.lcol, lwd = out.lwd) #ul
    
    if(view < 180)  
      segments(s$x, s$y, l$x, l$y, col = out.lcol, lwd = out.lwd) #sl    	    	
    
    if(view >= 80)
      segments(u$x, u$y, s$x, s$y, col = out.lcol, lwd = out.lwd) #us

    if(view >= 180)
      segments(l$x, l$y, m$x, m$y, col = out.lcol, lwd = out.lwd) #ml

  
  # Vertex points
  points(x = m$x, y = m$y, col = 'black', bg = 'mediumseagreen', pch = 21, cex = vertexsize)
  points(x = u$x, y = u$y, col = 'black', bg = 'darkorchid1', pch = 21, cex = vertexsize)
  points(x = s$x, y = s$y, col = 'black', bg = 'cornflowerblue', pch = 21, cex = vertexsize)
  points(x = l$x, y = l$y, col = 'black', bg = 'firebrick1', pch = 21, cex = vertexsize)
  
  # Save plot info 
#   .PlotTetraEnv <<- new.env()
   assign("last_plot.tetra", P, envir = .PlotTetraEnv)
   
}
