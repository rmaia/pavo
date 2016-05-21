#' Plot a Tetrahedral Colorspace
#'
#' \code{tcsplot} produces either a static or interactive 3D plot of a tetrahedral 
#' colorspace using OpenGL capabilities. Accessed via the function \code{\link{plot.colspace}}.
#'
#' @import rgl scatterplot3d
#' 
#' @param tcsdata (required) a data frame, possibly a result from the \code{colspace} 
#' function, containing values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param size size of the points in the plot (defaults to 0.02)
#' @param col color of the points in the plot (defaults to black)
#' @param alpha transparency of points (or volume fill in \code{tcsvol})
#' @param vertexsize size of the points at the vertices
#' @param achrosize size of the point in the achromatic center
#' @param achrocol color of the point in the achromatic center
#' @param lwd line width for the edges of the tetrahedron
#' @param lcol line color for the edges of the tetrahedron
#' @param new should a new 3D plot be called (defaults to \code{FALSE})?
#' @param hspin if \code{TRUE}, the graphic will spin horizontally (around the 'z' axis)(defaults to \code{FALSE})
#' @param vspin if \code{TRUE}, the graphic will spin vertically (around the 'x' axis)(defaults to \code{FALSE})
#' @param floor if \code{TRUE}, a reference xy plane is plotted under the tetrahedron (defaults to \code{TRUE})
#' @param grid if \code{TRUE}, connects the polygon outlining the volume occupied by points (defaults to \code{TRUE})
#' @param grid.alpha transparecny of the volume polygon grid lines
#' @param fill if \code{TRUE}, fills the volume occupied by points (WARNING: transparency
#' is not saved properly if exported using \code{rgl.postscript})(defaults to \code{TRUE}).
#' 
#' @return \code{tcsplot} creates a 3D plot using functions of the package \code{rgl}, 
#' based on openGL capabilities. Plot is interactive and can be manipulated with the mouse 
#' (left button: rotate along 'z' axis; right button: rotate along 'x' axis; 
#' third button: zoom). \code{tcsvol} creates polygon based on points, determining the volume
#' occupied by them in the colorspace. \code{tcspoints} adds points to the plot. Points are
#' currently plotted only as spheres to maintain export capabilities.
#'
#' @examples \dontrun{
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'avg.uv')
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#' plot(tcs.sicalis, size = 0.005)
#' rgl.postscript('testplot.pdf',fmt='pdf') 
#' rgl.snapshot('testplot.png')
#'
#' # For adding points
#' patch <- rep(c('C','T','B'),7)
#' tcs.crown <- subset(tcs.sicalis, 'C') 
#' tcs.breast <- subset(tcs.sicalis, 'B') 
#' plot(tcs.crown, col ='blue')
#' points(tcs.breast, col ='red')
#'
#' # For plotting convex hull
#' plot(tcs.sicalis, col = 'blue', size = 0.005)
#' vol(tcs.sicalis)
#' }
#' 
#' @seealso \code{\link{spheres3d}},\code{\link{rgl.postscript}}, 
#' \code{\link{rgl.snapshot}},\code{\link{rgl.material}} 
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
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

#ToDo: Add option to not plot tetrahedron

tcsplot<- function(tcsdata, size = 0.02, alpha = 1, col = 'black', 
                    vertexsize = 0.02, achro = TRUE, achrosize = 0.01, achrocol = 'grey', 
                    lwd = 1, lcol = 'lightgrey', new = FALSE, hspin = FALSE, 
                    vspin = FALSE, floor = TRUE, grid = TRUE, fill = TRUE, static = FALSE) {
  
  # Interactive 3D plot
  if(static == FALSE){
    # load RGL, and attempt install if not found
    loadrgl()
    
    if(new)
       open3d(FOV = 1, mouseMode = c('zAxis', 'xAxis', 'zoom'))
    
    # can't figure out how to change the character type
    
    ttv = pavo::ttvertex
    
    cu = t(col2rgb('#984EA3')) / 255
    cs = t(col2rgb('#377EB8')) / 255
    cm = t(col2rgb('#4DAF4A')) / 255
    cl = t(col2rgb('#E41A1C')) / 255
    
    plot3d(unlist(ttv[c('xu','xs','xm','xl')]),
    		unlist(ttv[c('yu','ys','ym','yl')]),
    		unlist(ttv[c('zu','zs','zm','zl')]), type = 's', lit = F,
    		radius = vertexsize, box = F, axes = F, xlab = '', ylab = '', zlab = '',
    		col = c(rgb(cu[1], cu[2], cu[3]), rgb(cs[1], cs[2], cs[3]), 
    		rgb(cm[1], cm[2], cm[3]), rgb(cl[1], cl[2], cl[3])))
    
    segments3d(ttv[c('xu','xs')], ttv[c('yu','ys')], ttv[c('zu','zs')], 
      color = lcol, lwd = lwd)
    segments3d(ttv[c('xu', 'xm')], ttv[c('yu', 'ym')], ttv[c('zu', 'zm')], 
      color = lcol, lwd = lwd)
    segments3d(ttv[c('xu', 'xl')], ttv[c('yu', 'yl')], ttv[c('zu', 'zl')], 
      color = lcol, lwd = lwd)
    segments3d(ttv[c('xs', 'xm')], ttv[c('ys', 'ym')], ttv[c('zs', 'zm')], 
      color = lcol, lwd = lwd)
    segments3d(ttv[c('xs','xl')], ttv[c('ys','yl')], ttv[c('zs','zl')], 
      color = lcol, lwd = lwd)
    segments3d(ttv[c('xl','xm')], ttv[c('yl', 'ym')], ttv[c('zl', 'zm')], 
      color = lcol, lwd = lwd)
    
    if(achro == TRUE)
      spheres3d(0, 0, 0, col = achrocol, radius = achrosize, lit = F)
    
    spheres3d(tcsdata[,c('x', 'y', 'z')], radius = size, color = col, alpha = alpha, lit = F)
    
    if(floor){
      vertices <- c( 
          -0.7, -0.5, -0.3, 1.0,
           0.7, -0.5, -0.3, 1.0,
           0.7,  1, -0.3, 1.0,
          -0.7,  1, -0.3, 1.0
      				)
      indices <- c(1, 2, 3, 4)
      
     wire3d(qmesh3d(vertices, indices), lit = F)
    	}
    	
    if(hspin)
       play3d(spin3d(axis = c(0, 0, 1), rpm = 20), duration = 3)
    
    if(vspin)
       play3d(spin3d(axis = c(1, 0, 0), rpm = 20), duration = 3)
  }
  
  # Static tetradehron
  # Can't work out how to plot points separately (i.e. OUTSIDE this function. As 
  # in plot(tcsdata) then points(moretcsdata). Scatterplot3d requires that plots be assigned
  # as objects, then points need to be added either using points3d (plot$points3d()), or
  # first projecting using plot$xyz.convers() then points(). Either way, need to access
  # the original plot object, and I don't know how to pass that back outside this function).
  # The defaults etc. aren't done properly yet either - I'll do that I've worked this out. 
  # Facing the same problem for the CIELAB plot which also uses scatterplot3d.
  if(static == TRUE){
    
    #library(scatterplot3d)
    
    plot <- scatterplot3d(0, 0, 0, box = TRUE, 
                          xlim = c(-1.22, 0.612), ylim = c(-0.35, 0.707), 
                          zlim = c(-0.25, 0.75), axis = F, grid = F, angle = 70, 
                          scale.y = 0.45, mar = c(1, 1, 1, 1))
    
    # Vertices
    u <- plot$xyz.convert(0, 0, 0.75)
    s <- plot$xyz.convert((-1 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25)
    m <- plot$xyz.convert(0, (1/sqrt(2)), -0.25)
    l <- plot$xyz.convert((0.5 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25)
    no.u <- plot$xyz.convert(0, 0, -0.25)
    
    # Draw it up
    segments(u$x, u$y, l$x, l$y)
    segments(u$x, u$y, m$x, m$y)
    segments(u$x, u$y, s$x, s$y)
    segments(s$x, s$y, l$x, l$y)
    segments(m$x, m$y, s$x, s$y)
    segments(l$x, l$y, m$x, m$y)
  
  # Origin
    if(achro == TRUE)
      plot$points3d(0, 0, 0, col = achrocol, cex = achrosize, pch = 15)
  
  # Data points
     plot$points3d(x = tcsdata$x, y = tcsdata$y, z = tcsdata$z, 
                   cex = 1, col = 'black', pch = 19)
  
  # Verticy points
  points(x = u$x, y = u$y, col = 'black', bg = 'darkorchid1', pch = 21)
  points(x = s$x, y = s$y, col = 'black', bg = 'cornflowerblue', pch = 21)
  points(x = m$x, y = m$y, col = 'black', bg = 'mediumseagreen', pch = 21)
  points(x = l$x, y = l$y, col = 'black', bg = 'firebrick1', pch = 21)
  }

}
