#' Plot a Tetrahedral Color Space
#'
#' Produces a 3D convex hull in tetrahedral color space when plotting a 
#' non-interactive tetrahedral plot
#'
#' @param tcsdata (required) object of class \code{colspace}.
#' @param alpha transparency of volume (if \code{fill = TRUE}).
#' @param grid logical. if \code{TRUE} (default), draws the polygon outline defined by the points.
#' @param fill logical. if \code{TRUE} (default), fills the volume defined by the points.
#' @param xlim,ylim,zlim,view,scale.y,margin,axis plotting parameters 
#' in case of a new plot. see \code{\link{tetraplot}}.
#' @param new logical. Should a new plot be started or draw over an open plot? 
#' (defaults to FALSE)
#' @param ... additional graphical options. See \code{link{polygon}}. 
#'
#' @return \code{vol} creates a 3D convex hull within a static tetrahedral plot
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export 
#' 

vol <- function(tcsdata, alpha = 0.2, grid = TRUE, fill = TRUE, 
                xlim = NULL, ylim = NULL, zlim = NULL, 
                view = NULL, scale.y = NULL, margin = NULL, 
                axis = FALSE, new = FALSE, ...){
  
  if(attr(tcsdata, 'clrsp') != 'tcs') stop("object is not in tetrahedral color space")
  
  vol <- t(convhulln(tcsdata[, c('x', 'y', 'z')], options = 'FA')$hull)
  coords <- tcsdata[, c('x', 'y', 'z')]
  listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
  ppairs <- do.call(rbind, lapply(listvol, function(x)t(combn(x, 2))))
  
  #check if there is a plot
  isthereplot <- try(get("last_plot.tetra", envir = .PlotTetraEnv), silent=TRUE)
  
  if(new){
  	if(is.null(xlim))
  	  xlim <- range(tcsdata[,'x'])
  	if(is.null(ylim))
  	  ylim <- range(tcsdata[,'y'])
  	if(is.null(zlim))
  	  zlim <- range(tcsdata[,'z'])
  	if(is.null(view))
  	  view <- 70
  	if(is.null(scale.y))
  	  scale.y <- 1
  	if(is.null(margin))
  	  margin <- c(1,1,1,1)
  	
  	# Empty plot
    P <- scatterplot3d(x=xlim, y=ylim, z=zlim, box = FALSE,
                                  axis = axis, grid = FALSE, angle = view, 
                                  scale.y = scale.y, mar = margin, pch = '')
    
      # Save plot info 
   .PlotTetraEnv <<- new.env()
   assign("last_plot.tetra", P, envir = .PlotTetraEnv)
  }
    
  
  last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
  flatcoords <- data.frame(last_tetraplot$xyz.convert(coords))

  arg <- list(...)
  
  if(is.null(arg$col))
    arg$col <- 'darkgrey'
  
  darkcolor <- arg$col
  alphacolor <- rgb(t(col2rgb(arg$col)), alpha=alpha*255, maxColorValue=255) 
        
  if(fill){
    arg$border <- NA
    arg$col <- alphacolor
    }
  
  if(grid)
    arg$border <- darkcolor
  
  if(!fill)
    arg$col <- NA
    
  for(i in 1:ncol(vol)){
  	arg$x <- flatcoords[vol[,i],'x']
  	arg$y <- flatcoords[vol[,i],'y']
  	
  	do.call(polygon, arg)
  	}
}