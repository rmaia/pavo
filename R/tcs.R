#' Tetracolorspace avian visual model
#'
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in the avian tetrahedral color space.
#'
#' @import geometry
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#' from \code{\link{vismodel}} or independently calculated data (in the form of a data frame
#' with four columns, representing the avian cones).
#'
#' @return A data frame of class \code{tcs} consisting of the following rows:
#' @return \code{u}, \code{s}, \code{m}, \code{l}: the quantum catch data used to
#' calculate the remaining variables. NOTE: even if visual sistem is of type V-VIS,
#' the output column will be labeled \code{u}.
#' @return \code{u.r}, \code{s.r}, \code{m.r}, \code{l.r}: relative cone stimulation,
#' for a given hue, as a function of saturation. See Stoddard & Prum (2008) for details.
#' @return \code{x}, \code{y}, \code{z}: cartesian coordinates for the points in the
#' tetrahedral color space.
#' @return \code{h.theta}, \code{h.phi}: angles theta and phi, in radians, determining
#' the hue of the color.
#' @return \code{r.vec}: the r vector (saturation, distance from the achromatic center).
#' @return \code{r.max}: the maximum r vector achievable for the color's hue.
#' @return \code{r.achieved}: the relative r distance from the achromatic center, in
#' relation to the maximum distance achievable (\code{r.vec/r.max})
#' @export
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual='avg.uv')
#' tcs.sicalis <- tcs(vis.sicalis)}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

tcs<- function(vismodeldata)
{

dat <- vismodeldata

# if object is vismodel:
if('vismodel' %in% attr(dat, 'class')){

# check if tetrachromat
  if(attr(dat, 'conenumb') < 4)
    stop('vismodel input is not tetrachromatic')

  if(attr(dat, 'conenumb') > 4)
    warning('vismodel input is not tetrachromatic, considering first four receptors only')
  
# check if relative
  if(!attr(dat, 'relative')){
    dat <- dat[,c('u','s','m','l')]/rowSums(dat[,c('u','s','m','l')])
    class(dat) <- class(vismodeldata)
    warning("Quantum catch are not relative, and have been transformed")
  }
    
}

# if not, check if it has more (or less) than 4 columns

if(!('vismodel' %in% attr(dat, 'class'))){

  if(ncol(dat) < 4)
    stop('Input data is not a ',  dQuote('vismodel'), ' object and has fewer than four columns')	
  if(ncol(dat) == 4)
    warning('Input data is not a ', dQuote('vismodel'), ' object; treating columns as standardized quantum catch for ', dQuote('u'),', ',  dQuote('s'),', ',  dQuote('m'),', and ', dQuote('l'), ' receptors, respectively')

  if(ncol(dat) > 4)
    warning('Input data is not a ', dQuote('vismodel'), ' object *and* has more than four columns; treating the first four columns as standardized quantum catch for ', dQuote('u'),', ',  dQuote('s'),', ',  dQuote('m'),', and ', dQuote('l'), ' receptors, respectively')
    
  dat <- dat[, 1:4]
  
  if(round(sum(rowSums(dat/apply(dat,1,sum)))) != dim(dat)[1]){
  	dat <- dat/apply(dat, 1, sum)
  	warning('Quantum catch are not relative, and have been divided by their sum')
  }
}


u <- dat[, 1]
s <- dat[, 2]
m <- dat[, 3]
l <- dat[, 4]

# cartesian coordinates

x <- ((1-2*s-m-u)/2)*sqrt(3/2)
y <- (-1+3*m+u)/(2*sqrt(2))
z <- u-(1/4)

# vertex cartesian coordinates & their spherical data

ttvx <- ttvertex

# spherical coordinates for the data points
# S&P suggest values with reflectance lower than a treshold (0.05) not have 
# hue & r. not implemented.

r.vec<- sqrt(x*x + y*y + z*z)
r.vec[r.vec==0] = NaN


h.theta<- atan2(y,x)
h.phi<- asin(z/r.vec)

#Rmax & Robtained

cosalpha.u <- cos(h.phi)*cos(ttvx$Huephi.u)*cos(h.theta-ttvx$Huetheta.u) +
  sin(h.phi)*sin(ttvx$Huephi.u)
cosalpha.s <- cos(h.phi)*cos(ttvx$Huephi.s)*cos(h.theta-ttvx$Huetheta.s) +
  sin(h.phi)*sin(ttvx$Huephi.s)
cosalpha.m <- cos(h.phi)*cos(ttvx$Huephi.m)*cos(h.theta-ttvx$Huetheta.m) +
  sin(h.phi)*sin(ttvx$Huephi.m)
cosalpha.l <- cos(h.phi)*cos(ttvx$Huephi.l)*cos(h.theta-ttvx$Huetheta.l) +
  sin(h.phi)*sin(ttvx$Huephi.l)

allcosalpha <- data.frame(cosalpha.u,cosalpha.s,cosalpha.m,cosalpha.l)

cosalphamax <- apply(allcosalpha,1,min)
r.max<- (0.25)/(-(cosalphamax))

r.achieved <- r.vec/r.max

# cone stimulation (for a given hue as a function of saturation, see S&P ESM)
# this is not really used, ever -- should we include it?

u.r<-r.vec*cosalpha.u
s.r<-r.vec*cosalpha.s
m.r<-r.vec*cosalpha.m
l.r<-r.vec*cosalpha.l

res.p <- data.frame(u, s, m, l, u.r , s.r, m.r, l.r, 
                  x, y, z, h.theta, h.phi, 
                  r.vec, r.max, r.achieved,
                  row.names=rownames(dat))

res <- res.p
# class(res) <- c('colorspace', 'data.frame')
class(res) <- c('tcs', 'data.frame')

attr(res, 'conenumb') <- 4

res
}
