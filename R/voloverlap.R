#' Color volume overlap
#'
#' calculates the overlap between the volumes defined by two sets of points in cartesian
#' space
#'
#' @export
#' @import rcdd
#' @param tcsres1,tcsres2 (required) data frame, possibly a result from the \code{tcs} 
#' function, containing
#' values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param plot Should the volumes and points be plotted? (defaults to \code{FALSE}.)
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



voloverlap <- function(tcsres1,tcsres2, plot=FALSE){

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

vol1 <- convhulln(dat1, 'FA')$vol
vol2 <- convhulln(dat2, 'FA')$vol

rat1 <- d2q(cbind(0, cbind(1, as.matrix(dat1))))
rat2 <- d2q(cbind(0, cbind(1, as.matrix(dat2))))
vert1 <- redundant(rat1, representation = "V")$output
vert2 <- redundant(rat2, representation = "V")$output
Hvert1 <- scdd(vert1, representation = "V")$output
Hvert2 <- scdd(vert2, representation = "V")$output
Hinter <- rbind(Hvert1, Hvert2)
Vinter <- scdd(Hinter, representation = "H")$output

Voverlap <- data.frame(q2d(Vinter[ , - c(1, 2)]))
names(Voverlap) = c('x','y','z')

if(dim(Voverlap)[1]>3){
  overlapVol <- convhulln(Voverlap, 'FA')$vol
  }else{
    overlapVol <- 0
    }

psmallest <- overlapVol/min(c(vol1,vol2))

pboth <- overlapVol/(sum(c(vol1,vol2))-overlapVol)

if(plot==T){
  open3d(FOV=1, mouseMode=c('zAxis','xAxis','zoom'))
  if(dim(Voverlap)[1]>3)
    ttvol(Voverlap)
  ttvol(dat1, col='red', fill=F)
  ttvol(dat2, col='blue', fill=F)
  }

data.frame(vol1, vol2, psmallest,pboth)

}