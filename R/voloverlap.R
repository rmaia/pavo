#' Color volume overlap
#'
#' calculates the overlap between the volumes defined by two sets of points in cartesian
#' space
#'
#' @export
#' @param tcsres1,tcsres2 (required) data frame, possibly a result from the \code{tcs} 
#' function, containing
#' values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param plot Should the volumes and points be plotted? (defaults to \code{FALSE})
#' @return Calculates the overlap between the volumes defined by two set of points in
#' colorspace. The volume from the overlap is then is given relative to:
#' \itemize{
#'	\item \code{vsmallest} the volume of the overlap divided by the smallest of that defined 
#' by the the two imput sets of color points. Thus, if one of the volumes is entirely 
#' contained within the other, this overlap will be \code{vsmallest = 1}.
#'  \item \code{vboth} the volume of the overlap divided by the combined volume of both 
#' imput sets of color points.
#' }
#' @note Stoddard & Stevens (2011) originally obtained the volume overlap through Monte Carlo
#' simulations of points within the range of the volumes, and obtaining the frequency of 
#' simulated values that fall inside the volumes defeined by both sets of color points.
#' @note Here we present an exact solution based on finding common vertices to both volumes
#' and calculating its volume.
#'
#' @note Stoddard & Stevens (2011) also return the value of the overlap relative to one of
#' the volumes (in that case, the host species). However, for other applications
#' this value may not be what one expects to obtain if (1) the two 
#' volumes differ considerably in size, or (2) one of the volumes is entirely contained
#' within the other. For this reason, we also report the volume relative to the union of
#' the two input volumes, which may be more adequate in most cases.
#' @examples \dontrun{
#' data(sicalis)
#' tcs.sicalis.C <- tcs(vismodel(sicalis[c(1,grep('\\.C',names(sicalis)))]))
#' tcs.sicalis.T <- tcs(vismodel(sicalis[c(1,grep('\\.T',names(sicalis)))]))
#' tcs.sicalis.B <- tcs(vismodel(sicalis[c(1,grep('\\.B',names(sicalis)))]))
#' voloverlap(tcs.sicalis.T,tcs.sicalis.B)
#' voloverlap(tcs.sicalis.T,tcs.sicalis.C, plot=T) }
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color in a tetrahedral color space: A phylogenetic analysis of new world buntings. The American Naturalist, 171(6), 755-776.
#' @references Stoddard, M. C., & Stevens, M. (2011). Avian vision and the evolution of egg color mimicry in the common cuckoo. Evolution, 65(7), 2004-2013.



voloverlap <- function(tcsres1,tcsres2, plot=FALSE){

dat1 <- tcsres1[, c('x', 'y', 'z')]

dat2 <- tcsres2[, c('x', 'y', 'z')]

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

vsmallest <- overlapVol/min(c(vol1,vol2))

vboth <- overlapVol/(sum(c(vol1,vol2))-overlapVol)

if(plot==T){
  open3d(FOV=1, mouseMode=c('zAxis','xAxis','zoom'))
  if(dim(Voverlap)[1]>3)
    ttvol(Voverlap)
  ttvol(dat1, col='red', fill=F)
  ttvol(dat2, col='blue', fill=F)
  }

data.frame(vol1, vol2, voverlap = overlapVol, vsmallest, vboth)

}