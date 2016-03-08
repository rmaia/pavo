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

# check class, import selected sensitivity
# make relative (in case not inherited relative)

# if(class(vismodeldata)=='vismodel'){
	# qcatch <- match.arg(qcatch)
	# dat <- data.frame(vismodeldata[qcatch])
	# names(dat) <- gsub(paste(qcatch,'.',sep=''), '', names(dat))
	
  # }else{
  	# dat <- vismodeldata
  	# }

dat <- vismodeldata
  	
if(any('v' %in% names(dat)))
  names(dat) <- gsub('v','u',names(dat))
  
# if the data frame has 4 columns, treat them as u/s/m/l
if(any(attr(dat, 'visualsystem')=='none') & ncol(dat) == 4 & !all(c('u','s','m','l') %in% names(dat))){

  names(dat) <- c('u','s','m','l')

  warning(paste('Input data has four columns, but they are not named',
  dQuote('u'),',',  dQuote('s'),',',  dQuote('m'),', and', dQuote('l'), '; treating them as such'))

}

if(!any(attr(dat, 'visualsystem')=='none') & ncol(dat) > 4 & !all(c('u','s','m','l') %in% names(dat))){

  names(dat)[1:4] <- c('u','s','m','l')

  warning(paste('Input data does not have columns named',
  dQuote('u'),',',  dQuote('s'),',',  dQuote('m'),', and', dQuote('l'), '; treating first four columns as such'))

}


  	
if(!all(c('u','s','m','l') %in% names(dat)))
  stop(paste('Input data must have columns named',
  dQuote('u'),',',  dQuote('s'),',',  dQuote('m'),', and', dQuote('l')))

if(!attr(dat, 'relative')){
  dat <- dat[,c('u','s','m','l')]/rowSums(dat[,c('u','s','m','l')])
  warning("Quantum catch are not relative, and have been transformed")
}

u <- dat[,'u']
s <- dat[,'s']
m <- dat[,'m']
l <- dat[,'l']

# cartesian coordinates

x <- ((1-2*s-m-u)/2)*sqrt(3/2)
y <- (-1+3*m+u)/(2*sqrt(2))
z <- u-(1/4)

# vertex cartesian coordinates & their spherical data

ttvx <- pavo::ttvertex

# spherical coordinates for the data points
# S&P suggest values with reflectance lower than a treshold (0.05) not have 
# hue & r . not implemented.

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
#names(res)[1:4] <- names(dat)

###################
#SUMMARY VARIABLES#
###################

# # if(!is.null(by)){
	
	# if(length(by)==1){
	# by.many <- by
	# by <- rep(1:(dim(res.p)[1]/by),each=by)
	# by <- factor(by,labels=row.names(res.p)[seq(1,length(row.names(res.p)),by=by.many)])
    # }

  # by <- factor(by)
  # res.c <- data.frame(t(sapply(levels(by),function(z)tcssum(res.p[which(by==z),]))))
  # row.names(res.c) <- levels(by)
	
# }else{
	# res.c <- data.frame(t(tcssum(res.p)))
	# row.names(res.c) <- 'all.points'
# }
#res <- list(tcs=res.p,summary=res.c)

res <- res.p
class(res) <- c('tcs', 'data.frame')
res
}
