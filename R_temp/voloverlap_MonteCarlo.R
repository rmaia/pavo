#' Color volume overlap
#'
#' calculates the overlap between the volumes defined by two sets of points in cartesian
#' space
#'
#' @export
#' @param tcsres1 (required) data frame, possibly a result from the \code{tcs} 
#' function, containing
#' values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param tcsres2 (required) data frame, possibly a result from the \code{tcs} 
#' function, containing
#' values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param nsamp number of simulated points
#' @param plot Should the volumes and points be plotted? (defaults to \code{FALSE}.)
#' @param size Size of the spheres in the 3D volume plot (if called)
#' @return Calculates the overlap between the volumes defined by two set of points in
#' colorspace. This is done by simulating points from a uniform distribution defined by
#' the combined values of the points, and obtaining the frequency of simulated values that
#' fall inside the volumes defeined by both sets of color points. This frequency is then
#' compared to (1) the frequency of values that fall within the smallest volume
#' (Stoddard & Stevens 2011), and (2) the combined volume of both sets of color points.
#'
#' @note Stoddard & Stevens (2011) calculate the volume overlap relative to one of the
#' volumes compared (i.e. how many of the simulated points that fall in volume 1 also 
#' fall in volume 2), and we return this value (which is always relative to the smallest
#' volume). However, this value may not be what one expects to obtain if (1) the two 
#' volumes differ considerably in size, or (2) one of the volumes is entirely contained
#' within the other. For this reason, we also report p(both)/(p(vol1)+p(vol2)), which
#' may be more adequate in those cases.
#' @note The simulation process requires the calculation of many convex hulls, and
#' therefore may be computationally intensive and take considerably long if many points
#' are simulated. Given that depending on the shape of the color volumes many of the points
#' simulated may fall outside either volumes, make sure to check the output to see if a
#' decent sample size is falling withing the volumes and being used in calculations.
#' @examples \dontrun{
#' data(sicalis)
#' tcs.sicalis.C <- tcs(vismodel(sicalis[c(1,grep('\\.C',names(sicalis)))]))
#' tcs.sicalis.T <- tcs(vismodel(sicalis[c(1,grep('\\.T',names(sicalis)))]))
#' tcs.sicalis.B <- tcs(vismodel(sicalis[c(1,grep('\\.B',names(sicalis)))]))
#' voloverlap(tcs.sicalis.T,tcs.sicalis.B, nsamp=5000)
#' voloverlap(tcs.sicalis.T,tcs.sicalis.C, plot=T) }
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Stoddard, M. C., & Stevens, M. (2011). Avian vision and the evolution of egg color mimicry in the common cuckoo. Evolution, 65(7), 2004-2013.



voloverlap <- function(tcsres1,tcsres2, nsamp=1000, plot=FALSE, size=0.001){

if(class(tcsres1)=='tcs'){
  dat1 <- tcsres1$tcs[, c('x', 'y', 'z')]	
  }else{
    dat1 <- tcsres1[, c('x', 'y', 'z')]
    }

if(class(tcsres2)=='tcs'){
  dat2 <- tcsres2$tcs[, c('x', 'y', 'z')]	
  }else{
    dat2 <- tcsres2[, c('x', 'y', 'z')]
    }

# calculate their volumes

vol1 <- convhulln(dat1, 'FA')$vol
vol2 <- convhulln(dat2, 'FA')$vol

# sample random points
pmin <- apply(rbind(dat1,dat2),2,min)
pmax <- apply(rbind(dat1,dat2),2,max)

samples <- apply(rbind(pmin,pmax), 2, function(x) runif(nsamp,x[1],x[2]))

sindex <- 1:dim(samples)[1]

newvol1 <- sapply(sindex, function(x) convhulln(rbind(dat1,samples[x,]),'FA')$vol)
newvol2 <- sapply(sindex, function(x) convhulln(rbind(dat2,samples[x,]),'FA')$vol)

# points that are within each volume

invol1 <- sapply(newvol1, function(x) isTRUE(x<=vol1))
invol2 <- sapply(newvol2, function(x) isTRUE(x<=vol2))

# how many points are in each category

s_in1 <- length(which(invol1))
s_in2 <- length(which(invol2))

s_inboth <- length(which(invol1 & invol2))

s_ineither <- length(which(invol1 | invol2))

# points in both relative points in smallest

psmallest <- s_inboth / c(s_in1,s_in2)[which.min(c(vol1,vol2))]

# points in both relative to total points in both

pboth <- s_inboth / s_ineither

if(plot==T){
  open3d(FOV=1, mouseMode=c('zAxis','xAxis','zoom'))
  ttvol(dat1, col='red', fill=F)
  ttvol(dat2, col='blue', fill=F)

  spheres3d(samples[which(invol1 & !invol2),], type='s', lit=F, radius=size, col='red')
  spheres3d(samples[which(invol2 & !invol1),], type='s', lit=F, radius=size, col='blue')  

  if(s_inboth > 0){  
    spheres3d(samples[which(invol1 & invol2),], type='s', lit=F, radius=size, col='black')
    }
  }

data.frame(vol1, vol2, s_in1,s_in2,s_inboth,s_ineither,psmallest,pboth)

}