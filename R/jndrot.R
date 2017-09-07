#' Rotate Cartesian coordinates obtained from \code{jnd2xyz}
#' 
#' Rotates the Cartesian coordinates obtained from \code{jnd2xyz}
#' 
#' @param jnd2xyzres (required) the output from a \code{jnd2xyz} call.
#' @param ref1 the cone to be used as a the first reference. Must match name
#' in the original data that was used for \code{coldist}. Defaults to 'l'.
#" (only used if data has 2 or 3 dimensions)
#' @param axis1 A vector of length 3 composed of 0's and 1's, with 
#' 1's representing the axes (x,y,z) to rotate around. Defaults to c(1,1,0), such
#' that the rotation aligns with the xy plane. (only used if data has 2 or 3 dimensions)
#' @param ref2 the cone to be used as a the second reference. Must match name
#' in the original data that was used for \code{coldist}. Defaults to 'u'.
#' (only used if data has 3 dimensions).
#' @param axis2 A vector of length 3 composed of 0's and 1's, with 
#' 1's representing the axes (x,y,z) to rotate around. Defaults to c(0,0,1), such
#' that the rotation aligns with the z axis. (only used if data has 3 dimensions).
#'
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers)
#' cd.flowers <- coldist(vis.flowers)
#' jndrot(jnd2xyz(cd.flowers))
#' }
#'
#' @author Rafael Maia \email{rm72@zips.uakron.edu}
#' 
#' @export
#'  



jndrot <- function(jnd2xyzres, ref1='l', axis1 = c(1,1,0), ref2='u', axis2 = c(0,0,1)){


  # TODO: OPTION TO CENTER ON DATA, NOT ACHROMATIC ROOT
  # TODO: "HIDE" REFERENCES FROM JND2XYZ OUTPUT
	
  if(!'jnd2xyz' %in% class(jnd2xyzres))
    stop('object must be a result from the "jnd2xyz" function')
  
  # helper functions
  vectormag <- function(x) sqrt(sum(x^2))
  vectornorm <- function(x) matrix(as.numeric(x/vectormag(x)), ncol=1)
  vectorcross <- function(a,b){
    i = c(2,3,1)
    j = c(3,1,2)
    a[i]*b[j] - a[j]*b[i]
  }
  
  coords <- as.matrix(jnd2xyzres)
  
  # one dimension
  if(round(sum(c('x','y','z') %in% colnames(coords))) == 1){
  	
    if(coords[dim(coords)[1],'x'] < coords[dim(coords)[1]-1,'x'])
      coords[,'x'] <- coords[,'x']*-1
    
    res <- coords
  }
  
  # two dimensions
  if(round(sum(c('x','y','z') %in% colnames(coords))) == 2){
  	
  	coords <- cbind(coords, 0)

    if(length(axis1) !=3)
      stop('"axis1" must be a vector of length 3')
      
    aa <- vectornorm(coords[grep(paste0('jnd2xyzrrf.',ref1), rownames(coords)), ] - 
                     coords['jnd2xyzrrf.achro', ])
    bb <- vectornorm(axis1)
    daabb <- sum(aa*bb)
    ncaabb <- vectormag(vectorcross(aa,bb))
    GG <- rbind(c(daabb, -ncaabb,0),
                c(ncaabb, daabb, 0),
                c(0,0,1))
    FF <- cbind(aa, 
                vectornorm(bb - daabb*aa),
                vectorcross(bb,aa))

    RR <- FF %*% GG %*% solve(FF)

   res <- sweep(coords, 2, coords['jnd2xyzrrf.achro',], '-')
   res <- t(apply(res, 1, function(x) RR %*% x))
   #res <- sweep(res, 2, coords['jnd2xyzrrf.achro',], '+')
   
   res <- res[,seq(dim(res)[2]-1)]
   colnames(res) <- colnames(jnd2xyzres)
  }

  # three dimensions
  if(round(sum(c('x','y','z') %in% colnames(coords))) == 3){
  	
    # first rotation
    
    if(length(axis1) !=3)
      stop('"axis1" must be a vector of length 3')
      
    aa <- vectornorm(coords[grep(paste0('jnd2xyzrrf.',ref1), rownames(coords)), ] - 
                     coords['jnd2xyzrrf.achro', ])
    bb <- vectornorm(axis1)
    daabb <- sum(aa*bb)
    ncaabb <- vectormag(vectorcross(aa,bb))
    GG <- rbind(c(daabb, -ncaabb,0),
                c(ncaabb, daabb, 0),
                c(0,0,1))
    FF <- cbind(aa, 
                vectornorm(bb - daabb*aa),
                vectorcross(bb,aa))

    RR <- FF %*% GG %*% solve(FF)

   res <- sweep(coords, 2, coords['jnd2xyzrrf.achro',], '-')
   res <- t(apply(res, 1, function(x) RR %*% x))
   #res <- sweep(res, 2, coords['jnd2xyzrrf.achro',], '+')
   
   
    # second rotation
    
    if(length(axis2) !=3)
      stop('"axis2" must be a vector of length 3')
      
    aa <- vectornorm(res[grep(paste0('jnd2xyzrrf.',ref2), rownames(res)), ] - 
                     res['jnd2xyzrrf.achro', ])
    bb <- vectornorm(axis2)
    daabb <- sum(aa*bb)
    ncaabb <- vectormag(vectorcross(aa,bb))
    GG <- rbind(c(daabb, -ncaabb,0),
                c(ncaabb, daabb, 0),
                c(0,0,1))
    FF <- cbind(aa, 
                vectornorm(bb - daabb*aa),
                vectorcross(bb,aa))

    RR <- FF %*% GG %*% solve(FF)

   res <- sweep(res, 2, res['jnd2xyzrrf.achro',], '-')
   res <- t(apply(res, 1, function(x) RR %*% x))
   #res <- sweep(res, 2, coords['jnd2xyzrrf.achro',], '+')
   
   colnames(res) <- colnames(jnd2xyzres)
   
  }

res <- as.data.frame(res)
class(res) <- class(jnd2xyzres)

res
}