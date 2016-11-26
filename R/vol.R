#' Plot a Tetrahedral Color Space
#'
#' \code{vol} produces a 3D convex hull in tetrahedral color space when plotting a 
#' non-interactive tetrahedral plot
#'
#' @param colspacedata (required) and object of class \code{colspace}.
#' @param alpha transparency of volume (if \code{fill = TRUE}).
#' @param grid logical. if \code{TRUE} (default), draws the polygon outline defined by the points.
#' @param fill logical. if \code{TRUE} (default), fills the volume defined by the points.
#' @param ... additional graphical options. See \code{link{polygon}}. 
#'
#' @return \code{vol} creates a 3D convex hull within a static tetrahedral plot
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export 
#' 

vol <- function(colspacedata, alpha = 0.2, grid = TRUE, fill = TRUE, ...){
  
  if(attr(colspacedata, 'clrsp') != 'tcs') stop("object is not in tetrahedral color space")
  
  vol <- t(convhulln(colspacedata[, c('x', 'y', 'z')], options = 'FA')$hull)
  coords <- colspacedata[, c('x', 'y', 'z')]
  listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
  ppairs <- do.call(rbind, lapply(listvol, function(x)t(combn(x, 2))))
  
  last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
  flatcoords <- data.frame(last_tetraplot$xyz.convert(coords))

  arg <- list(...)
  
  if(is.null(arg$col))
    arg$col <- 'darkgrey'
    
  if(is.character(arg$col))
    arg$col <- rgb(t(col2rgb(col)), alpha=alpha*255, maxColorValue=255) 
    
  if(!grid)
    arg$border <- NA
    
  if(!fill)
    arg$border <- arg$col
    arg$col <- NA
    
  for(i in 1:ncol(vol)){
  	arg$x <- flatcoords[vol[,i],'x']
  	arg$y <- flatcoords[vol[,i],'y']
  	
  	do.call(polygon, arg)
  	}
}