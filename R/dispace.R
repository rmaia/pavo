#' Dichromatic colour space
#' 
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in a dichromatic colour space.
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#'  with two columns representing trichromatic viewer).
#' 
#' @return A data frame of class \code{colorspace} consisting of the following columns:
#' @return \code{s}, \code{l}: the quantum catch data used to calculate 
#'  the remaining variables.
#' @return \code{x}: the coordinate of the stimulus along a segment
#' @return \code{r.vec}: the r vector (saturation, distance from the center).
#' 
#' @export
#' 
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- dispace(vis.flowers)
#' } 
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision - 
#'  behavioural tests and physiological concepts. Biological Reviews, 78,
#'  81 - 118.

dispace <- function(vismodeldata){
  
  dat <- vismodeldata
    
# if object is vismodel:
  if('vismodel' %in% attr(dat, 'class')){
    
# check if trichromat
    if(attr(dat, 'conenumb') < 2)
      stop('vismodel input is not dichromatic')
    
    if(attr(dat, 'conenumb') > 2)
      warning('vismodel input is not dichromatic, considering first two receptors only')
    
# check if relative
    if(!attr(dat, 'relative')){
      dat <- dat[, c('s','l')]/rowSums(dat[, c('s','l')])
      class(dat) <- class(vismodeldata)
      warning("Quantum catch are not relative, and have been transformed")
    }
    
  }
    
# if not, check if it has more (or less) than 2 columns
  
  if(!('vismodel' %in% attr(dat, 'class'))){
    
    if(ncol(dat) < 2)
      stop('Input data is not a ',  dQuote('vismodel'), ' object and has fewer than two columns')	
    if(ncol(dat) == 2)
      warning('Input data is not a ', dQuote('vismodel'), ' object; treating columns 
              as standardized quantum catch for ', dQuote('s'), ' and ', dQuote('l'), 
              ' receptors, respectively')
    
    if(ncol(dat) > 2)
      warning('Input data is not a ', dQuote('vismodel'), ' object *and* has more than two columns; 
              treating the first two columns as standardized quantum catch for ', 
              dQuote('s'),', and ', dQuote('l'), ' receptors, respectively')
    
    dat <- dat[, 1:2]
    
    if(round(sum(rowSums(dat/apply(dat,1,sum)))) != dim(dat)[1]){
      dat <- dat/apply(dat, 1, sum)
      warning('Quantum catch are not relative, and have been divided by their sum')
    }
  }
    
  s <- dat[, 1]
  l <- dat[, 2]
  
# coordinate
  x <- (1/sqrt(2)) * (l - s) 
    
# colorimetrics?
  r.vec <- abs(x)
  
  res.p <- data.frame(s, l, x, r.vec, row.names = rownames(dat))
  
  res <- res.p
  
  class(res) <- c('colorspace', 'data.frame')
  
  attr(res, 'conenumb') <- 2
  attr(res, 'clrsp') <- 'dispace'
  
  res
}
