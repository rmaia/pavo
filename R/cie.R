#' CIE colour spaces
#' 
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in either the CIEXYZ (1931) or CIELAB (1971) colour space
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#'  with three columns representing trichromatic viewer).
#' @param space (required) Choice between XYZ (1931) or LAB (1971) colour models.
#' 
#' @return Object of class \code{colorspace} containing:
#'    \itemize{
#'      \item \code{X, Y, Z}: Tristimulus values. 
#'      \item \code{x, y, z}: Cartesian coordinates, when using \code{space = XYZ}.
#'      \item \code{L, a, b}: Lightness, \code{L}, and colour-opponent \code{a} 
#'          (redness-greenness) and \code{b} (yellowness-blueness) values, in a 
#'          Cartesian coordinate space. Returned when using \code{space = LAB}.
#'    }
#'    
#' @export
#' 
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'cie2')
#' flowers.cie <- cie(vis.flowers, space = 'XYZ')
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @references Smith T, Guild J. (1932) The CIE colorimetric standards and their use.
#'    Transactions of the Optical Society, 33(3), 73-134.
#' @references Westland S, Ripamonti C, Cheung V. (2012). Computational colour science 
#'    using MATLAB. John Wiley & Sons.
#' @references Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of 
#'  the middle- and long-wavelength sensitive cones derived from measurements in 
#'  observers of known genotype. Vision Research, 40, 1711-1737.
#' @references CIE (2006). Fundamental chromaticity diagram with physiological axes. 
#'  Parts 1 and 2. Technical Report 170-1. Vienna: Central Bureau of the Commission 
#'  Internationale de l' Éclairage.

cie <- function(vismodeldata, space = c('XYZ', 'LAB'), 
                illuminant = c('bluesky', 'forestshade', 'D65', 'white')){
  
  dat <- vismodeldata  

  # Coordinates in the chosen CIE space
  if(identical(space, 'XYZ')){
    x <- dat$X / (dat$X + dat$Y + dat$Z)
    y <- dat$Y / (dat$X + dat$Y + dat$Z)
    z <- dat$Z / (dat$X + dat$Y + dat$Z)
  }else if(identical(space, 'LAB')){
    # Tristimulus values for neutral point
    Qn <- data.frame(Xn = sum(rep(1, 401) * vis[, 1] * illum),
                     Yn = sum(rep(1, 401) * vis[, 2] * illum),
                     Zn = sum(rep(1, 401) * vis[, 3] * illum))
    f <- function(x){
                     if(isTRUE(x > (6/29)^3)){
                       x^(1/3)
                     }else{
                       (841/108) * x + (4/29)
                     }
    }
    if(isTRUE(Q$Y/Qn$Yn > 0.008856)){model$L <- 116*f(Q$Y/Qn$Yn)-16}else{model$L  <- 903.3*(Q$Y/Qn$Yn)}
    model$a <- 500 * (f(Q$X/Qn$Xn) - f(Q$Y/Qn$Yn))
    model$b <- 200 * (f(Q$Y/Qn$Yn) - f(Q$Z/Qn$Zn))
  }
  
  # todo: for CIELCh space 
  #model$C_ab <- sqrt(model$a^2 + model$b^2)
  #model$h_ab <-  atan(model$b/model$a)*(180/pi)
  
  res.p <- data.frame(dat, x, y, z, row.names = rownames(dat))
  
  res <- res.p
  
  class(res) <- c('colorspace', 'data.frame')
  
  attr(res, 'conenumb') <- 3
  attr(res, 'clrsp') <- paste0('CIE', space)
  
  res
}