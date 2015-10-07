#' Maxwell triangle
#' 
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in a Maxwell triangle chromaticity space.
#' 
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#' from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#' with three columns representing trichromatic viewer).
#' 
#' @return A data frame of class \code{colorspace} consisting of the following rows:
#' @return \code{s}, \code{m}, \code{l}: the quantum catch data used to calculate 
#' the remaining variables.
#' @return \code{x}, \code{y}: cartesian coordinates for the points in the
#' Maxwell triangle.
#' @return \code{h.theta}: angle theta, in radians, determining the hue of the color.
#' @return \code{r.vec}: the r vector (saturation, distance from the achromatic center).
#' @export
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' maxwell.flowers <- maxwell(vis.flowers)
#' } 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @references Maxwell JC. (1970). On color vision. In: Macadam, D. L. (ed) 
#' Sources of Color Science. Cambridge, MIT Press, 1872 - 1873.
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision - 
#' behavioural tests and physiological concepts. Biological Reviews, 78,
#' 81 - 118.
#' @references Neumeyer C (1980) Simultaneous color contrast in the honeybee. 
#' Journal of comparative physiology, 139(3), 165-176.

maxwell <- function(vismodeldata){
  
  dat <- vismodeldata
  
# if object is vismodel:
if('vismodel' %in% attr(dat, 'class')){
  
# check if trichromat
  if(attr(dat, 'conenumb') < 3)
    stop('vismodel input is not trichromatic')
  
  if(attr(dat, 'conenumb') > 3)
    warning('vismodel input is not trichromatic, considering first three receptors only')
  
# check if relative
  if(!attr(dat, 'relative')){
    dat <- dat[, c('s','m','l')]/rowSums(dat[, c('s','m','l')])
    class(dat) <- class(vismodeldata)
    warning("Quantum catch are not relative, and have been transformed")
  }
  
}
  
# if not, check if it has more (or less) than 4 columns

if(!('vismodel' %in% attr(dat, 'class'))){
  
  if(ncol(dat) < 3)
    stop('Input data is not a ',  dQuote('vismodel'), ' object and has fewer than four columns')	
  if(ncol(dat) == 3)
    warning('Input data is not a ', dQuote('vismodel'), ' object; treating columns 
            as standardized quantum catch for ', dQuote('s'),', ',  dQuote('m'),
            ', and ', dQuote('l'), ' receptors, respectively')
  
  if(ncol(dat) > 3)
    warning('Input data is not a ', dQuote('vismodel'), ' object *and* has more than four columns; 
            treating the first three columns as standardized quantum catch for ', 
            dQuote('s'),', ',  dQuote('m'),', and ', dQuote('l'), ' receptors, respectively')
  
  dat <- dat[, 1:3]
  
  if(round(sum(rowSums(dat/apply(dat,1,sum)))) != dim(dat)[1]){
    dat <- dat/apply(dat, 1, sum)
    warning('Quantum catch are not relative, and have been divided by their sum')
  }
}
  
s <- dat[, 1]
m <- dat[, 2]
l <- dat[, 3]

# cartesian coordinates
  
x <- (1/sqrt(2)) * (l - m) 
y <- (sqrt(2)/sqrt(3)) * (s - ((l + m)/2))   
  
# colorimetrics
r.vec <- sqrt((abs(x))^2 + (abs(y)^2))
h.theta <- atan2(y, x)

res.p <- data.frame(s, m, l, x, y, h.theta, r.vec, row.names = rownames(dat))

res <- res.p

class(res) <- c('colorspace', 'data.frame')

attr(res, 'conenumb') <- 3
attr(res, 'clrsp') <- 'maxwell'

res
}
