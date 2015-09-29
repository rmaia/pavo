#' Plot a Tetrahedral Color Space
#'
#' \code{tcspoints} plots points in a tetrahedral color space
#'
#' @return \code{tcspoints} creates 3D points in a tetrahedral color space plot produced by \code{tcsplot}
#' using functions of the package \code{rgl}, based on openGL capabilities.
#'
#' @rdname tcsplot
#'
#' @export

tcspoints<- function(tcsdata, size=0.02, col='black', alpha=1){

# load RGL, and attempt install if not found
loadrgl()

spheres3d(tcsdata[,c('x','y','z')], radius=size, color=col, lit=F, alpha=alpha)
}
