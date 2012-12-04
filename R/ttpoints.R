#' Plot a Tetrahedral Color Space
#'
#' \code{ttpoints} plots points in a tetrahedral color space
#'
#' @return \code{ttplot} creates 3D points in a tetrahedral color space plot produced by \code{ttplot}
#' using functions of the package \code{rgl}, based on openGL capabilities.
#'
#' @rdname ttplot
#'
#' @export

ttpoints<- function(tcsdata, size=0.02, col='black'){

# if(class(tcsdata)=='tcs'){
  # dat <- tcsdata$tcs  
  # }else{
    # dat <- tcsdata
    # }


spheres3d(tcsdata[,c('x','y','z')], radius=size, color=col, lit=F)
}
