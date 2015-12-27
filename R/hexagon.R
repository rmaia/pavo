#' Colour hexagon
#' 
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in the hymenopteran color hexagon.
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#'  with three columns representing trichromatic viewer).
#' 
#' @return A data frame of class \code{colorspace} consisting of the following columns:
#' @return \code{s}, \code{m}, \code{l}: the quantum catch data used to calculate 
#'  the remaining variables
#' @return \code{x}, \code{y}: cartesian coordinates in the colour hexagon
#' @return \code{h.theta}: hue angle theta (in degrees), with 0-degrees at the 1200
#'  angle, increasing clockwise
#' @return \code{r.vec}: the r vector (saturation, distance from the center)
#' @return \code{sec.fine}: fine 'hue sector', wherein the full hexagon is composed
#'  of 36 10-degree sectors, with 0-degrees at the 1200 angle (Chittka et al.1994)
#' @return \code{sec.coarse}: coarse 'hue sector', wherein the full hexagon is 
#'  composed of five sectors: blue, bluegreen, green, uvgreen, uv, and uvblue 
#'  (Chittka et al. 1994)
#' 
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')
#' flowers.hex <- hexagon(vis.flowers)
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'    based on photoreceptor excitations as a generalized representation of 
#'    colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Chittka L, Shmida A, Troje N, Menzel R. (1994). Ultraviolet as a 
#'    component of flower reflections, and the colour perception of Hymenoptera. 
#'    Vision research, 34(11), 1489-1508.

hexagon <- function(vismodeldata){
  
## Note: requires von kries & hyperbolic transform
  
  dat <- vismodeldata
  
# if object is vismodel:
  if('vismodel' %in% attr(dat, 'class')){
    
    # check if trichromat
    if(attr(dat, 'conenumb') < 3)
      stop('vismodel input is not trichromatic')
    
    if(attr(dat, 'conenumb') > 3)
      warning('vismodel input is not trichromatic, considering first three receptors only')
    
    # check if relative. Qcatches, at this stage, need to be raw & not log-transformed 
    # for the hexagon as it uses a hyperbolic transform. 
    if(attr(dat, 'relative'))
      stop("Quantum catches are relative, which is not required in the hexagon model")
    
    if(attr(dat, 'qcatch') != 'Ei')  # todo: more flexible
      stop("Quantum catches are not hyperbolically transformed, as required for the hexagon model")
    
    if(!isTRUE(attr(dat, 'vonkries')))
      stop("Quantum catches are not von-Kries transformed, as required for the hexagon model")
  }
    
# if not, check if it has more (or less) than 3 columns
  
  if(!('vismodel' %in% attr(dat, 'class'))){
    
    if(ncol(dat) < 3)
      stop('Input data is not a ',  dQuote('vismodel'), ' object and has fewer than three columns')	
    if(ncol(dat) == 3)
      warning('Input data is not a ', dQuote('vismodel'), ' object; treating columns 
              as quantum catch for ', dQuote('s'),', ',  dQuote('m'),
              ', and ', dQuote('l'), ' receptors, respectively')
    
    if(ncol(dat) > 3)
      warning('Input data is not a ', dQuote('vismodel'), ' object *and* has more than three columns; 
              treating the first three columns as quantum catch for ', 
              dQuote('s'),', ',  dQuote('m'),', and ', dQuote('l'), ' receptors, respectively')
    
    dat <- dat[, 1:3]
    
    if(round(sum(rowSums(dat/apply(dat,1,sum)))) == dim(dat)[1])
      stop("Quantum catches are relative, which is not required in the hexagon model and may produce unexpected results")
  }
  
  s <- dat[, 1]
  m <- dat[, 2]
  l <- dat[, 3]
    
# Hexagon coordinates & colorimetrics
  x <- (sqrt(3)/2) * (l - s)
  y <- m - (0.5 * (s + l))
  
# colorimetrics
  r.vec <- sqrt((abs(x))^2 + (abs(y)^2))
  h.theta <- sapply(1:length(x), function(i) angle360(x[i], y[i]))
  sec.fine <- round(floor(h.theta/10), 0) * 10
  sec.coarse <- sapply(1:length(x), function(x) coarse_sec(h.theta[x]))
  
  res.p <- data.frame(s, m, l, x, y, h.theta, r.vec, sec.fine, sec.coarse, row.names = rownames(dat))
  
  res <- res.p
  
  class(res) <- c('colorspace', 'data.frame')
  
  attr(res, 'conenumb') <- 3
  attr(res, 'clrsp') <- 'hexagon'
  
  res
}
