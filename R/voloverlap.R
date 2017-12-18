#' Color volume overlap
#'
#' Calculates the overlap between the volumes defined by two sets of points in cartesian
#' space.
#'
#' @import rcdd
#' 
#' @export
#' 
#' @param tcsres1,tcsres2 (required) data frame, possibly a result from the \code{colspace} 
#' function, containing
#' values for the 'x', 'y' and 'z' coordinates as columns (labeled as such)
#' @param plot logical. Should the volumes and points be plotted? (defaults to \code{FALSE})
#' @param interactive logical. If \code{TRUE}, uses the rgl engine for interactive plotting;
#' if \code{FALSE} then a static plot is generated.
#' @param col a vector of length 3 with the colors for (in order) the first volume, 
#' the second volume, and the overlap.
#' @param fill logical. should the two volumes be filled in the plot? (defaults to \code{FALSE})
#' @param new logical. Should a new plot window be called? If \code{FALSE}, volumes and their
#' overlap are plotted over the current plot (defaults to \code{TRUE}).
#' @param montecarlo logical. If \code{TRUE}, Monte Carlo simulation is used instead of exact
#' solution (not recommended; defaults to \code{FALSE})
#' @param nsamp if \code{montecarlo = TRUE}, determines the number of points to be sampled.
#' @param psize if \code{montecarlo = TRUE} and \code{plot = TRUE}, sets the size to plot the points
#' used in the Monte Carlo simulation.
#' @param lwd if \code{plot = TRUE}, sets the line width for volume grids.
#' @param ... additional arguments passed to the plot. See \code{\link{vol}}
#' @return Calculates the overlap between the volumes defined by two set of points in
#' colorspace. The volume from the overlap is then given relative to:
#' \itemize{
#'	\item \code{vsmallest} the volume of the overlap divided by the smallest of that defined 
#' by the the two input sets of color points. Thus, if one of the volumes is entirely 
#' contained within the other, this overlap will be \code{vsmallest = 1}.
#'  \item \code{vboth} the volume of the overlap divided by the combined volume of both 
#' input sets of color points.
#' }
#'
#' The Monte Carlo solution is available mostly for legacy and benchmarking, and is not recommended
#' (see notes). If used, the output will be different:

#' \itemize{
#' 	\item \code{s_in1, s_in2} the number of sampled points that fall within each of the volumes 
#' individually.
#' 	\item \code{s_inboth} the number of sampled points that fall within both volumes.
#' 	\item \code{s_ineither} the number of points that fall within either of the volumes.
#' 	\item \code{psmallest} the proportion of points that fall within both volumes divided by the 
#'  number of points that fall within the smallest volume.
#'	\item \code{pboth} the proportion of points that fall within both volumes divided by the total 
#'  number of points that fall within both volumes.
#'	}
#'	
#' If the Monte Carlo solution is used, a number of points much greater than the default should be
#' considered (Stoddard & Stevens(2011) use around 750,000 points, but more or fewer might be required
#' depending on the degree of overlap).
#'
#' @note Stoddard & Stevens (2011) originally obtained the volume overlap through Monte Carlo
#' simulations of points within the range of the volumes, and obtaining the frequency of 
#' simulated values that fall inside the volumes defined by both sets of color points.
#' @note Here we present an exact solution based on finding common vertices to both volumes
#' and calculating its volume. However, we also the Monte Carlo solution is available through
#' the \code{montecarlo=TRUE} option.
#'
#' @note Stoddard & Stevens (2011) also return the value of the overlap relative to one of
#' the volumes (in that case, the host species). However, for other applications
#' this value may not be what one expects to obtain if (1) the two 
#' volumes differ considerably in size, or (2) one of the volumes is entirely contained
#' within the other. For this reason, we also report the volume relative to the union of
#' the two input volumes, which may be more adequate in most cases.
#' 
#' @examples \dontrun{
#' data(sicalis)
#' tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), 'C')
#' tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), 'T')
#' tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), 'B')
#' voloverlap(tcs.sicalis.T, tcs.sicalis.B)
#' voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = T)
#' voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = T, col = 1:3) }
#' 
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}, with code from Sebastien Villeger
#' 
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color 
#' in a tetrahedral color space: A phylogenetic analysis of new world buntings. The 
#' American Naturalist, 171(6), 755-776.
#' @references Stoddard, M. C., & Stevens, M. (2011). Avian vision and the evolution of 
#' egg color mimicry in the common cuckoo. Evolution, 65(7), 2004-2013.
#' @references Villeger, S., Novack-Gottshall, P. M., & Mouillot, D. (2011). The 
#' multidimensionality of the niche reveals functional diversity changes in benthic 
#' marine biotas across geological time. Ecology Letters, 14(6), 561-568.


voloverlap <- function(tcsres1, tcsres2, plot = FALSE, interactive = FALSE,
              col = c('blue', 'red', 'darkgrey'), fill=FALSE, new = TRUE,
              montecarlo = FALSE, nsamp = 1000, psize = 0.001, 
              lwd = 1, ...){

dat1 <- tcsres1[, c('x', 'y', 'z')]

dat2 <- tcsres2[, c('x', 'y', 'z')]

vol1 <- convhulln(dat1, 'FA')$vol
vol2 <- convhulln(dat2, 'FA')$vol

######################
#EXACT SOLUTION BEGIN#
######################
if(!montecarlo){
rat1 <- d2q(cbind(0, 1, as.matrix(dat1)))
rat2 <- d2q(cbind(0, 1, as.matrix(dat2)))
Hvert1 <- scdd(rat1, representation = "V")$output
Hvert2 <- scdd(rat2, representation = "V")$output
Hinter <- rbind(Hvert1, Hvert2)
Vinter <- scdd(Hinter, representation = "H")$output

Voverlap <- data.frame(q2d(Vinter[ , - c(1, 2)]))
colnames(Voverlap) <- c('x','y','z')

if(nrow(Voverlap)>3){
  overlapVol <- convhulln(Voverlap, 'FA')$vol
  }else{
    overlapVol <- 0
    }

vsmallest <- overlapVol/min(vol1,vol2)

vboth <- overlapVol/(sum(vol1,vol2)-overlapVol)

res <- data.frame(vol1, vol2, overlapvol = overlapVol, vsmallest, vboth)
}

####################
#EXACT SOLUTION END#
####################

###################
#MONTE CARLO BEGIN#
###################

if(montecarlo){
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

res <- data.frame(vol1, vol2, s_in1,s_in2,s_inboth,s_ineither,psmallest,pboth)
}

#################
#MONTE CARLO END#
#################

############
#PLOT BEGIN#
############
if(plot){
    if(length(col)<3)
      col <- c(rep(col,2)[1:2], 'darkgrey')
      
      
	if(interactive){
	    # check if rgl is installed and loaded
        if (!requireNamespace("rgl", quietly = TRUE))
          stop(dQuote('rgl'),' package needed for interactive plots. Please install it, or use interactive=FALSE.',
            call. = FALSE)  

	  if(!isNamespaceLoaded("rgl"))
	    requireNamespace("rgl")
	    
	  if(new)
        rgl::open3d(FOV=1, mouseMode=c('zAxis','xAxis','zoom'))

      tcsvol(tcsres1, col=col[1], fill=F)
      tcsvol(tcsres2, col=col[2], fill=F)
      
      if(!montecarlo){
        if(dim(Voverlap)[1]>3){
        	  attr(Voverlap, 'clrsp') <- "tcs"
          tcsvol(Voverlap, col=col[3])
          }
        }
      
      if(montecarlo){
        rgl::spheres3d(samples[which(invol1 & !invol2),], type='s', 
          lit=F, radius=psize, col=col[1])
        rgl::spheres3d(samples[which(invol2 & !invol1),], type='s', 
          lit=F, radius=psize, col=col[2])  

        if(s_inboth > 0){  
          rgl::spheres3d(samples[which(invol1 & invol2),], type='s', 
          lit=F, radius=psize, col=col[3])
        }
      }
	}
	
	if(!interactive){
      plotrange <- apply(rbind(tcsres1[,c('x','y','z')],tcsres2[,c('x','y','z')]),2,range)

    if(length(fill)<3)

      if(dim(Voverlap)[1]>3){
        vol(Voverlap, col=col[3], new=new, fill=TRUE,
          xlim=plotrange[,'x'], ylim=plotrange[,'y'], 
          zlim=plotrange[,'z'], lwd=lwd, ...)
        vol(tcsres1, col=col[1], fill=fill, lwd=lwd, new=FALSE)
        vol(tcsres2, col=col[2], fill=fill, lwd=lwd, new=FALSE)
      }else{
        vol(tcsres1, col=col[1], lwd=lwd, new=new, fill=fill,
          xlim=plotrange[,'x'], ylim=plotrange[,'y'], 
          zlim=plotrange[,'z'], ...)
        vol(tcsres2, col=col[2], lwd=lwd, fill = fill, new=FALSE)
      }
	}
	
    
##########
#PLOT END#
##########    
}

return(res)
}
