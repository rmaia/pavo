#' Add Legend to a Static Tetrahedral Colorspace
#' \code{legendtetra} adds legend to a 
#' static tetrahedral colorspace plot.
#' 
#' @param location a vector containing x, y and z values for the position of
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


legendtetra <- function(location = c(-1.4, -0.3, 0.7), ...){
	last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
	
	if(length(location) != 3) stop('location must have values for x, y and z', call.=FALSE)
	
	arg <- list(...)
	
	position <- last_tetraplot$xyz.convert(location[1],location[2],location[3])
	
	arg$x <- position$x
	arg$y <- position$y
	
	do.call(legend, arg)	
}