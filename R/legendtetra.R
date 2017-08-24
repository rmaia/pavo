#' Add legend to a static tetrahedral colorspace
#' 
#' Adds a legend to a static tetrahedral colorspace plot.
#' 
#' @param x,y position of the legend relative to plot limits
#'  (usually a value between 0 and 1, but because of the perspective distortion,
#' values greater than 1 are possible)
#' @param ... additional arguments passed to \code{\link{legend}}.
#'
#' @return \code{legendtetra} adds a legend to astatic tetrahedral colorspace plot. 
#' for additional information on which areguments are necessary and how they are used,
#' see \code{\link{legend}}.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export


legendtetra <- function(x=0.8, y=1.2, ...){
  
	arg <- list(...)
	
	arg$x <- grconvertX(x,"npc")
	arg$y <- grconvertY(y, "npc")
	
	do.call(legend, arg)	
	
}