#' Plot Points in a Colorspace
#'
#' \code{points.colorspace} plots additional points in a colorspace
#'
#' @return \code{points.colorspace} adds points to a colorspace plot. When \code{space = 'tcs'}, 
#'  it creates 3D points in a tetrahedral color space plot using functions of the package \code{rgl}, 
#'  based on openGL capabilities.
#'
#' @rdname plot.colorspace
#'
#' @export

points.colorspace <- function(clrspdata, ...){
  
  if(attr(clrspdata, 'clrsp') != 'tcs'){
    
    # Defaults in line with those in the plots
    arg <- list(...)
    
    if(is.null(arg$col))
      arg$col <- 'forestgreen'
    if(is.null(arg$pch))
      arg$pch <- 19
    arg$x <- clrspdata$x
    arg$y <- clrspdata$y
    
    do.call(points, arg)
  }
  
  if(attr(clrspdata, 'clrsp') == 'tcs'){
    .tcspoints(clrspdata, ...)
  }
  
}