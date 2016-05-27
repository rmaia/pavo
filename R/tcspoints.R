#' Plot a Tetrahedral Color Space
#'
#' \code{tcspoints} plots points in a tetrahedral color space
#'
#' @return \code{tcspoints} creates 3D points in a tetrahedral color space plot produced by \code{tcsplot}
#' using functions of the package \code{rgl}, based on openGL capabilities.
#'
#' @rdname tcsplot
#'

.tcspoints<- function(tcsdata, size = 0.02, col = 'black', alpha = 1){
  
  if(attr(tcsdata, 'clrsp') != 'tcs') stop("object is not in tetrahedral color space")
  
  # load RGL, and attempt install if not found
  #loadrgl()
  if(!isNamespaceLoaded("rgl"))
    requireNamespace("rgl")
  
  rgl::spheres3d(tcsdata[, c('x', 'y', 'z')], 
    radius = size, color = col, lit = F, alpha = alpha)
}
