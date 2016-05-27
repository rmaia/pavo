#' Plot Points in a Colorspace
#'
#' \code{points.colspace} plots additional points in a colorspace
#' 
#' @param x (required) an object of class \code{colspace}. 
#' @param ... additional graphical options. See \code{\link{par}}.
#'
#' @return \code{points.colspace} adds points to a colorspace plot. When \code{space = 'tcs'}, 
#'  it creates 3D points in a tetrahedral color space plot using functions of the package \code{rgl}, 
#'  based on openGL capabilities.
#'
#' @export

points.colspace <- function(x, ...){
  
  if(attr(x, 'clrsp') != 'tcs'){
    
    # Defaults in line with those in the plots
    arg <- list(...)
    
    if(is.null(arg$col))
      arg$col <- 'forestgreen'
    if(is.null(arg$pch))
      arg$pch <- 19
      arg$x <- x[ ,'x']
      arg$y <- x[ ,'y']
    
    do.call(points, arg)
  }
  
  if(attr(x, 'clrsp') == 'tcs'){
    .tcspoints(x, ...)
  }
  
}