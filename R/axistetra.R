#' Plot Reference Axes in a Static Tetrahedral Colorspace
#' \code{axistetra} plots reference x, y and z arrows showing the direction of
#' the axes in a static tetrahedral colorspace plot.
#' 
#' @param x,y,z coordinates for the origin position of the arrows
#' @param size length of the arrows. Can be either a single value 
#' (applied for x, y and z) or a vector of 3 separate values for each axis
#' @param arrowhead size of the arrowhead
#' @param col,lty,lwd graphical parameters for the arrows
#' @param label logical, include x, y and z labels (defaults to TRUE)
#' @param label.adj position adjustment for the labels. Can be either a 
#' single value (applied for x, y and z) or a vector of 3 separate values
#' for each axis
#' @param label.cex,label.col graphical parameters for the labels
#'
#' @return \code{axistetra} adds reference arrows showing the direction of the 
#' 3-dimensional axes in a static tetrahedral colorspace plot. 
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export

axistetra <- function(x=0.6, y=0.8, z=0.7, size=0.1, 
                      arrowhead=0.05, col = par("fg"), 
                      lty = par("lty"), lwd = par("lwd"),
                      label=TRUE, label.adj=0.03, label.cex=1, label.col=NULL){
  if(length(size) > 1){
  	lx <- size[1]
  	ly <- size[2]
  	lz <- size[3] 
  } else{
  	lx <- ly <- lz <- size[1]
  }
  
  last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
  
  xarr <- last_tetraplot$xyz.convert(c(x, x+lx), c(y,y), c(z,z))
  yarr <- last_tetraplot$xyz.convert(c(x, x), c(y,y+ly), c(z,z))
  zarr <- last_tetraplot$xyz.convert(c(x, x), c(y,y), c(z,z+lz))
  
  arrows(xarr$x[1], xarr$y[1], xarr$x[2], xarr$y[2], length=arrowhead, 
    lty=lty, lwd=lwd, col=col)
  arrows(yarr$x[1], yarr$y[1], yarr$x[2], yarr$y[2], length=arrowhead, 
    lty=lty, lwd=lwd, col=col)
  arrows(zarr$x[1], zarr$y[1], zarr$x[2], zarr$y[2], length=arrowhead, 
    lty=lty, lwd=lwd, col=col)
  
  if(label){

  if(length(label.adj) > 1){
  	px <- 1+label.adj[1]
  	py <- 1+label.adj[2]
  	pz <- 1+label.adj[3]
  } else{
  	px <- py <- pz <- 1+label.adj[1]
  }
 	
  	text(xarr$x[2]*px, xarr$y[2], labels='x', cex=label.cex, col=label.col)
  	text(yarr$x[2]*py, yarr$y[2]*py, labels='y', cex=label.cex, col=label.col)
  	text(zarr$x[2], zarr$y[2]*pz, labels='z', cex=label.cex, col=label.col)
  	
#pj <- 1 + label.adj  	
#text(xarr$x[2]*pj,xarr$y[2], 'x')
#text(yarr$x[2]*pj,yarr$y[2]*pj, 'y')
#text(zarr$x[2],zarr$y[2]*pj, 'z')

  }
}

