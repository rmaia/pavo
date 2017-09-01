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
#' @param perspective logical, should perspective be forced by using point size to reflect
#' distance from the plane of view? (defaults to TRUE)
#' @param range, vert.range multiplier applied to \code{cex} and \code{vert.cex}, respectively, 
#' to indicate the size range variation reflecting the distance from the plane of view.
#' @param r the distance of the eyepoint from the centre of the plotting box. 
#' Very high values approximate an orthographic projection (defaults to 1e6).
#' See \code{\link{persp}} for details.
#' @param zoom zooms in (values greater than 1) or out (values between 0 and 1) from the plotting area.
#' @param achro logical. Should the achromatic center be plotted? (defaults to \code{TRUE})
#' @param achro.line logical. Should the achromatic line be plotted? (defaults to \code{FALSE})
#' @param achro.col, achro.size, achro.lwd, achro.lty graphical parameters for the achromatic coordinates.
#' @param tetrahedron logical. Should the tetrahedron be plotted? (defaults to \code{TRUE})
#' @param vert.cex size of the points at the vertices (defaults to 1).
#' @param out.lwd, out.lcol graphical parameters for the tetrahedral outline.
#' @param margin vector of four numbers specifying drawing margins (defaults to c(0, 0, 0, 0)).
#' @param type accepts a vector of length 1 or 2 with 'p' for points and/or 'l' for lines from the point to
#' the base of the tetrahedron.
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

tetraplot <- function(tcsdata, theta = 45, phi = 10, perspective = TRUE, 
  range = c(1, 2), r = 1e6, zoom = 1, 
  achro = TRUE, achro.col = 'grey', achro.size = 1, achro.line = FALSE, achro.lwd = 1, achro.lty = 3,
  tetrahedron = TRUE, vert.cex = 1, vert.range = c(1,2) ,out.lwd = 1, out.lcol = 'darkgrey',
  margin = c(0,0,0,0), type='p', view, scale.y, axis, grid, vertexsize, ...) {
    	
  # check deprecated arguments view, scale.y, axis, grid
  if(!missing(view))
    stop('argument "view" is deprecated, please use "theta" and "phi" instead. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(scale.y))
    stop('argument "scale.y" is deprecated, please use "expand" instead. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(axis))
    stop('argument "axis" is deprecated. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(grid))
    stop('argument "grid" is deprecated. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
  if(!missing(vertexsize))
    stop('argument "vertexsize" is deprecated, please use "vert.cex" instead. see ?plot.colspace or ?tetraplot for more information.', call.=FALSE)
    
  trange <- function(x, newmin, newmax) 
    (((x - min(x)) * (newmax - newmin)) / (max(x) - min(x))) + newmin

  # get arguments
  arg <- list(...)
    
  if(is.null(arg$col)) arg$col <- 1
  if(is.null(arg$cex)) arg$cex <- 1
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
  rownames(sides) <- paste0(rep(
     do.call(paste0, data.frame(t(combn(c('u','s','m','l'), 2)))),each=2),
     c('.1', '.2'))

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
  # Save rotation matrix without plotting

  pdf(file=NULL)
  M <- do.call(persp, c(list(x=arg$xlim,
                             y=arg$ylim,
                             z=matrix(c(arg$zlim,arg$zlim), nrow=2),
                             border=FALSE, r=r, box=FALSE, theta=theta, phi=phi), arg))
  garbage <- dev.off()

  # position of points in projected space
  allcoords <- as.matrix(rbind(
    tcsdata[,c('x','y','z')], 
    verts[,c('x','y','z')], 
    achro=c(0,0,0), 
    achrbot=c(0,0,-0.25)
    ))
    
  tcoord <- cbind(allcoords, 1) %*% M
  tcoord[ ,1] <- tcoord[ ,1]/tcoord[ ,4] 
  tcoord[ ,2] <- tcoord[ ,2]/tcoord[ ,4] 
  colnames(tcoord) <- c('x', 'y', 'depth', 'scale')
  
  # Empty plot
  argblank <- arg

  argblank[names(as.list(args(graphics:::persp.default)))] <- NULL  
  argblank$xlim <- tcoord['achro','x'] + c(-1,1)*max(abs(tcoord['achro','x'] - tcoord[,'x'])) / zoom
  argblank$ylim <- tcoord['achro','y'] + c(-1,1)*max(abs(tcoord['achro','y'] - tcoord[,'y'])) / zoom
  #argblank$ylim <- range(tcoord[,'y'])
  argblank$x <- tcoord
  argblank$type <- 'n'
  argblank$bty <- 'n'
  argblank$xaxt <- 'n'
  argblank$yaxt <- 'n'
  argblank$ylab <- ''
  argblank$xlab <- ''
  
  par(mar=margin, pty='s')  
  do.call(plot, argblank)
  
  # Get point coordinates
  xy <- tcoord[rownames(tcoord) %in% rownames(tcsdata), c('x','y'), drop=FALSE]

  # get depth vector
  if(tetrahedron){
  	dvals <- tcoord[,'depth']
  } else{
  	dvals <- tcoord[!rownames(tcoord) %in% c('u','s','m','l'),'depth']
  }
  
  # transform depth vector
  dvals <- trange(dvals, range[1], range[2])
  
  # square root so it scales by area
  dvals <- sqrt(dvals)
  dvals <- trange(dvals, range[1], range[2])
  
  maxdatad <-  max(dvals[dvals[names(dvals) %in% rownames(tcsdata)]]) 
  
  # turn depth vector to point size
  	psize <- dvals*arg$cex
  	
  	vrange <- vert.cex*vert.range
  	psize[c('u','s','m','l')] <- trange(psize[c('u','s','m','l')], vrange[1], vrange[2])
  	
  	# distort if ranges are not the same for points and vertices
  	if(!identical(range, vert.range)){
  	  psize[names(psize) %in% rownames(tcsdata)] <- 
  	    trange(psize[names(psize) %in% rownames(tcsdata)], range[1], range[2])
  	}
  	  

  	if(!perspective){
  	  psize[] <- arg$cex
  	  psize[c('u','s','m','l')] <- vert.cex
  	}

  
  # add tetrahedron lines and vertices behind the data
  
  if(tetrahedron){
  	
    # vertice colors
    vcols <- setNames(c('darkorchid1','cornflowerblue','mediumseagreen', 'firebrick1'), rownames(verts))   
    
    # tetrahedron sides
    xytet <- cbind(sides, 1) %*% M
    xytet[ ,1] <- xytet[ ,1]/xytet[ ,4] 
    xytet[ ,2] <- xytet[ ,2]/xytet[ ,4] 
    colnames(xytet) <- c('x', 'y', 'depth', 'scale')

    # which vertex are behind data
    vinback <- dvals[c('u','s','m','l')] < maxdatad
    
    segs <- cbind(xytet[c(1,3,5,7,9,11), c('x','y')], xytet[c(2,4,6,8,10,12), c('x','y')])
    
    linback <- apply(do.call(rbind, 
      lapply(names(vinback)[vinback], grepl, x=rownames(segs))), 2, all)
    
    segments(
      segs[linback,1,drop=F],
      segs[linback,2,drop=F],
      segs[linback,3,drop=F],
      segs[linback,4,drop=F],
      lwd = out.lwd, col = out.lcol
      )
    
    # add vertices behind tetrahedron
 
    points(tcoord[names(vinback)[vinback], c('x','y'), drop=FALSE], pch=21, 
      cex = psize[names(vinback)[vinback]], col=NULL, 
      bg=vcols[names(vinback)[vinback]])
      
  }
  

  # add achromatic center if it is behind the data
  if(achro && dvals["achro"] < maxdatad)
    points(tcoord['achro',c('x','y'), drop=FALSE], col=NULL, bg=achro.col, pch=21, cex=psize['achro'])
    
  # add achromatic line if behind the data
  if(achro.line && dvals["achro"] < maxdatad)
    lines(tcoord[c('achrbot','u'),c('x','y'), drop=FALSE], col=achro.col, lty=achro.lty, lwd=achro.lwd)
  
  ######################
  # add tcsdata points #
  ######################
  argpoints <- arg

  argpoints[names(as.list(args(graphics:::persp.default)))] <- NULL
  argpoints['col'] <- col

  argpoints$cex <- psize[names(psize) %in% rownames(tcsdata)]
  
  argpoints$x <- xy
  
   if('l' %in% type){
   	 # calculate bottom of the points
     botpoints <- as.matrix(tcsdata[, c('x','y','z')])
     botpoints[,'z'] <- -0.25
   	 botpoints <- cbind(botpoints, 1) %*% M
   	 botpoints[ ,1] <- botpoints[ ,1]/botpoints[ ,4] 
     botpoints[ ,2] <- botpoints[ ,2]/botpoints[ ,4] 
     colnames(botpoints) <- c('x', 'y', 'depth', 'scale')

  	 arghl <- argpoints
  	 arghl$x <- NULL
  	 arghl$x0 <- xy[,'x']
  	 arghl$y0 <- xy[,'y']
  	 arghl$x1 <- botpoints[,'x']
  	 arghl$y1 <- botpoints[,'y']
  	 do.call(segments, arghl)
   }

  if('p' %in% type)
    do.call(points, argpoints)  
  
  # add achromatic center if it is in front of the data
  if(achro && dvals["achro"] > maxdatad)
    points(tcoord['achro',c('x','y'), drop=FALSE], col=NULL, bg=achro.col, pch=21, cex=psize['achro'])
    
  # add achromatic line if in front of the data
  if(achro.line && dvals["achro"] > maxdatad)
    lines(tcoord[c('achrbot','u'),c('x','y'), drop=FALSE], col=achro.col, lty=achro.lty, lwd=achro.lwd)
  
  # add tetrahedron lines and vertices in front of the points
  if(tetrahedron){  
    segments(
      segs[!linback,1,drop=F],
      segs[!linback,2,drop=F],
      segs[!linback,3,drop=F],
      segs[!linback,4,drop=F],
      lwd = out.lwd, col = out.lcol
      )
 
    points(tcoord[names(vinback)[!vinback], c('x','y'), drop=FALSE], pch=21, 
      cex = psize[names(vinback)[!vinback]], col=NULL, 
      bg=vcols[names(vinback)[!vinback]])
   
  }


 # Save plot info 
 assign("last_plot.tetra", M, envir = .PlotTetraEnv)  
}
