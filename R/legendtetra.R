#' Add legend to a static tetrahedral colorspace
#' 
#' Adds a legend to a static tetrahedral colorspace plot.
#' 
#' @param x,y,z a vector containing x, y and z values for the position of
#' the legend.
#' @param ... additional arguments passed to \code{\link{legend}}.
#'
#' @return \code{legendtetra} adds a legend to astatic tetrahedral colorspace plot. 
#' for additional information on which areguments are necessary and how they are used,
#' see \code{\link{legend}}.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export


legendtetra <- function(x=-1.4, y=-0.3, z=0.7, ...){
  
	last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
	
	arg <- list(...)
	
	position <- last_tetraplot$xyz.convert(x,y,z)
	
	arg$x <- position$x
	arg$y <- position$y
	
	do.call(legend, arg)	
	
}