#' Plot a static tetrahedral colorspace
#'
#' Produces a static 3D tetrahedral plot.
#' 
#' 
# #' @usage plot(tcsdata, ...) 
#' 
#' @param tcsdata (required) a data frame, possibly a result from the \code{colspace} 
#' or \code{tetraspace} function, containing values for the 'x', 'y' and 'z' 
#' coordinates as columns (labeled as such).
#' @param theta angle to rotate the plot in the xy plane (defaults to 10).
#' @param phi angle to rotate the plot in the yz plane (defaults to 45).
#' @param r the distance of the eyepoint from the centre of the plotting box. 
#' See \code{\link{persp}} for details.
#' @param zoom zooms in (values greater than 1) or out (values between 0 and 1) from the plotting area.
#' @param achro logical. Should the achromatic center be plotted? (defaults to \code{TRUE})
#' @param achro.line logical. Should the achromatic line be plotted? (defaults to \code{FALSE})
#' @param achro.col, achro.size, achro.lwd, achro.lty graphical parameters for the achromatic coordinates.
#' @param tetrahedron logical. Should the tetrahedron be plotted? (defaults to \code{TRUE})
#' @param out.lwd, out.lcol graphical parameters for the tetrahedral outline.
#' @param vertex.size size of the points at the vertices (defaults to 0.8).
#' @param box logical. Should the plot area box and axes be plotted? (defaults to \code{FALSE})
#' @param margin vector of four numbers specifying drawing margins (defaults to c(0, 0, 0, 0)).
#' @param view, scale.y, axis, grid deprecated arguments.
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
#' @author Chad Eliason \email{cme16@zips.uakron.edu}
#'
#' @export
#' 
#' @keywords internal
#'
#' @importFrom grDevices trans3d
#' @importFrom graphics persp
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage 
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings. 
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns 
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

tetraplot <- function(tcsdata, theta = 45, phi = 10, r = 12, zoom = 1, 
  achro = TRUE, achro.col = 'grey', achro.size = 0.8, achro.line = FALSE, achro.lwd = 1, achro.lty = 3,
  tetrahedron = TRUE, out.lwd = 1, out.lcol = 'darkgrey', vertexsize = 0.8, box = FALSE,
  margin = c(0,0,0,0), view, scale.y, axis, grid, ...) {
    	
  # check deprecated arguments view, scale.y, axis, grid
  if(!missing(view))
    stop('argument "view" is deprecated, please use "theta" and "phi" instead. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(scale.y))
    stop('argument "scale.y" is deprecated, please use "expand" instead. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(axis))
    stop('argument "axis" is deprecated, please use "box" instead. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(grid))
    stop('argument "grid" is deprecated. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)

  # get arguments
  arg <- list(...)
    
  if(is.null(arg$col)) arg$col <- 1
  if(is.null(arg$pch)) arg$pch <- 19
  if(is.null(arg$xlab)) arg$xlab <- 'x'    
  if(is.null(arg$ylab)) arg$ylab <- 'y'
  if(is.null(arg$zlab)) arg$zlab <- 'z'
        
  col <- arg['col']
  arg['col'] <- NULL
    

  
  # tetrahedron vertices
  verts <- matrix(c(
  0, 0, 0.75,
  (-0.5 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25,
  0, (1/sqrt(2)), -0.25,
  (0.5 * sqrt(1.5)), (-1/(2 * sqrt(2))), -0.25
  ), byrow=TRUE, nrow=4, 
  dimnames= list(c('u','s','m','l'), c('x','y','z')))
  
  # combinations of vertices to make facets
  sides <- verts[combn(1:4, 2), ] 
  
  # if no limits are given, estimate based on tetrahedron or tcsdataa limits
  if(any(sapply(list(arg$xlim, arg$ylim, arg$zlim), is.null))){

  	# first check if all xyzlim are null
  	if(!all(sapply(list(arg$xlim, arg$ylim, arg$zlim), is.null)))
  	  stop('"xlim", "ylim" and "zlim" must either all be NULL or all be vectors of length 2', call.=FALSE)
  	
  	if(tetrahedron){
  	  arg$xlim <- range(verts[,'x']) / zoom
  	  arg$ylim <- range(verts[,'y']) / zoom
  	  arg$zlim <- range(verts[,'z']) / zoom
  	} else{
  	  arg$xlim <- range(tcsdata[,'x'])
  	  arg$ylim <- range(tcsdata[,'y'])
  	  arg$zlim <- range(tcsdata[,'z'])
  	}

  }


  # draw blank 3d plot

  par(mar=margin)
  
  M <- do.call(persp, c(list(x=arg$xlim,
                             y=arg$ylim,
                             z=matrix(c(arg$zlim,arg$zlim), nrow=2),
                             border=FALSE, r=r, box=box, theta=theta, phi=phi), arg))
  
  # add tetrahedron
  if(tetrahedron){
  	xytet <- trans3d(sides[,'x'], sides[,'y'], sides[,'z'], M)
  	lines(xytet, lwd = out.lwd, col = out.lcol)
  	  	
  	if(theta > 0 & theta <= 120)
  	  lines(xytet$x[-c(5:6)], xytet$y[-c(5:6)], lwd = out.lwd, col = out.lcol)
  	  
  	# theta between 120 and 250: um in front
  	if(theta > 120 & theta <= 250)
  	  lines(xytet$x[-c(3:4)], xytet$y[-c(3:4)], lwd = out.lwd, col = out.lcol)
  	
  	# theta between 250 and 360: us in front
  	if(theta > 250 & theta <= 360)
  	  lines(xytet$x[-c(1:2)], xytet$y[-c(1:2)], lwd = out.lwd, col = out.lcol)
  	  
  }
  
  # add achromatic center
  if(achro)
    points(trans3d(0, 0, 0, M), col=NULL, bg=achro.col, pch=21, cex=achro.size)
  
  # add achromatic line
  if(achro.line)
    lines(trans3d(c(0,0), c(0,0), c(-.25,.75), M), col=achro.col, lty=achro.lty, lwd=achro.lwd)
  

  
  argpoints <- arg
#  argpoints[c('xlim', 'ylim', 'zlim', 'xlab', 'ylab', 'zlab', 
#              'main', 'sub', 'd', 'scale', 'expand', 'border', 'ltheta',
#              'lphi', 'shade', 'box', 'axes', 'nticks', 'ticktype')] <- NULL


  # add tcsdata points
  
  argpoints[names(as.list(args(graphics:::persp.default)))] <- NULL
  argpoints['col'] <- col
    
  xy <- trans3d(tcsdata[,'x'], tcsdata[,'y'], tcsdata[,'z'], M)

  do.call(points, c(xy, argpoints))
    
  # add tetrahedron lines in front of the points
  if(tetrahedron){
  	# theta between 0 and 120: ul in front
  	if(theta > 0 & theta <= 120)
  	  lines(xytet$x[c('u','l')], xytet$y[c('u','l')], lwd = out.lwd, col = out.lcol)
  	  
  	# theta between 120 and 250: um in front
  	if(theta > 120 & theta <= 250)
  	  lines(xytet$x[c('u','m')], xytet$y[c('u','m')], lwd = out.lwd, col = out.lcol)
  	
  	# theta between 250 and 360: us in front
  	if(theta > 250 & theta <= 360)
  	  lines(xytet$x[c('u','s')], xytet$y[c('u','s')], lwd = out.lwd, col = out.lcol)

  	
    # add vertex points
    xyvert <- trans3d(verts[,'x'], verts[,'y'], verts[,'z'], M)
    points(xyvert, pch=21, cex = vertexsize, col=NULL, 
      bg=c('darkorchid1','cornflowerblue','mediumseagreen', 'firebrick1'))
	
  }

 # Save plot info 
 assign("last_plot.tetra", M, envir = .PlotTetraEnv)  
}
