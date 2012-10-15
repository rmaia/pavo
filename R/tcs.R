#' Tetracolorspace avian visual model
#'
#' calculates coordinates and colorimetric variables that represent reflectance spectra
#' in the avian tetrahedral color space.
#'
#' @param vismodeldata (required) Quantum catch color data. Can be either the result
#' from \code{vismodel} or independently calculated data (in the form of a data frame
#' with four columns, representing the avian cones).
#' @param qcatch Quantum catch values to use in the model. Can be either \code{Qi}, 
#' \code{qi} or \code{fi} (defaults to \code{Qi}).
#' 
#' @return a data frame consisting of the following rows:
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
#' #INCLUDE EXAMPLE}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns as birds see them. Biological Journal Of The Linnean Society, 86(4), 405–431.

tcs<- function(vismodeldata, qcatch=c('Qi','qi','fi'))
{

if(is.list(vismodeldata)){
	qcatch <- match.arg(qcatch)
	dat <- data.frame(vismodeldata[qcatch])
  }
  
# make relative (in case not inherited relative)

dat <- dat/rowSums(dat)

u <- dat[,1]
s <- dat[,2]
m <- dat[,3]
l <- dat[,4]

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

# centroid
centroid <- colMeans(res.p[c('u','s','m','l')])

# color span
colspan.m <- mean(dist(res.p[,c('x','y','z')]))
colspan.v <- var(dist(res.p[,c('x','y','z')]))

# color volume

#if(nrow(res.p)>3)
#     {
     c.vol <- convhulln(res.p[,c('x','y','z')],"FA")$vol
#     }else{
#     	warning('Not enough color points to calculate volume (min 4)', call.=FALSE)
#     	Color.vol<-NA}

# hue disparity

hdisp.m <- mean(huedisp(res.p))
hdisp.v <- var(huedisp(res.p))

# summary of achieved chroma

mean.ra <- mean(res.p$r.achieved)
max.ra  <-  max(res.p$r.achieved)

res.c <- c(centroid,colspan.m,colspan.v,mean.ra,max.ra)
names(res.c) <- c('centroid.u', 'centroid.s' ,'centroid.m' ,'centroid.l' ,
                'colspan.m', 'colspan.v', 'mean.ra', 'max.ra')


res<-list(tcs=res.p,summary=res.c)

}