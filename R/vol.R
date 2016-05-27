#' Plot a 3D Convex Hull in a Tetrahedral Colorspace
#'
#' \code{vol} produces a 3D convex hull in tetrahedral color space (i.e. when 
#' \code{space = 'tcs'})
#' 
#' @param x (required) an object of class \code{colspace}. 
#' @param ... additional graphical options. See \code{\link{par}}.
#'
#' @return \code{vol.colspace} creates a 3D convex hull within a \code{plot.colspace} object, 
#'  only when \strong{\code{space = 'tcs'}}. 
#'
#' @export 

vol <- function(x, ...){
  
  if(attr(x, 'clrsp') != 'tcs')
  	if(!all(c('x','y','z') %in% names(x)))
      stop("object is not in tetrahedral color space")
  
  .tcsvol(x, ...)
  
}
