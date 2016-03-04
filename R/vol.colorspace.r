#' Plot a 3D Convex Hull in a Tetrahedral Colorspace
#'
#' \code{vol.colorspace} produces a 3D convex hull in tetrahedral color space (i.e. when 
#' \strong{\code{space = 'tcs'}})
#'
#' @return \code{vol.colorspace} creates a 3D convex hull within a \code{plot.colorspace} object, 
#'  when \strong{\code{space = 'tcs'}}, 
#'  
#' @rdname plot.colorspace
#'
#' @export

vol.colorspace <- function(clrspdata, ...){
  
  if(attr(clrspdata, 'clrsp') == 'tcs'){
    .tcsvol(clrspdata, ...)
  }
  
}