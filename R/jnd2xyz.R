#' Convert JND distances into perceptually-corrected Cartesian coordinates
#' 
#' Converts a \code{coldist} output into Cartesian coordinates that are
#' perceptually-corrected (i.e. Euclidean distances = JND distances)
#' 
#' @param coldistres (required) the output from a \code{coldist} call.
#'
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers)
#' cd.flowers <- coldist(vis.flowers)
#' jnd2xyz(cd.flowers)
#' }
#'
#' @author Rafael Maia \email{rm72@zips.uakron.edu}
#' 
#' @export
#'  

jnd2xyz <- function(coldistres) {
 
  # Accessory functions
  pos3 <- function(d12, d13, d23){
	x3 <- (d13^2 - d23^2 + d12^2)/(2*d12)
	y3 <- rep(0, 2)
	y3sq <- d13^2 - x3^2
	if(y3sq > 0)
	  y3 <- sqrt(y3sq)*c(1,-1)
	
	matrix(c(rep(x3,2),y3), ncol=2, dimnames=list(NULL, c('x','y')))
  }

  pos4 <- function(d12, d14, d24, d34){
	x4 <- (d14^2 - d24^2 + d12^2)/(2*d12)
	y4 <- ((d14^2 - d34^2 + thirdpointxy['y']^2 + thirdpointxy['x']^2)/(2*thirdpointxy['y'])) -
	      (x4*(thirdpointxy['x']/thirdpointxy['y']))
	z4 <- rep(0, 2)
	z4sq <- d14^2 - x4^2 - y4^2
	if(z4sq > 0)
	  z4 <- sqrt(z4sq)*c(1,-1)
	matrix(c(rep(x4,2), rep(y4,2), z4), ncol=3, dimnames=list(NULL, c('x','y','z')))
  }
  
  references <- attr(coldistres, 'resref')
  references <- references[intersect(
    grep('jnd2xyzrrf', references$patch1, invert=T), 
    grep('jnd2xyzrrf', references$patch2)
    ),]
    
  combined <- rbind(coldistres, references)

  cdmat <- coldist2mat(combined)[['dS']]

  coords <- matrix(NA, nrow=nrow(cdmat), ncol=3, dimnames=list(row.names(cdmat), c('x','y', 'z')))
  
  ptnames <- rownames(coords)

  # first point
  coords[ptnames[1], ] <- c(0,0,0)
  
  # second point
  coords[ptnames[2], ] <- c(cdmat[ptnames[1], ptnames[2]],0,0)
  
  # third point
  thirdpointxy <- pos3(cdmat[ptnames[1], ptnames[2]], 
                       cdmat[ptnames[1], ptnames[3]], 
                       cdmat[ptnames[2], ptnames[3]])[1, ]
                       
  coords[ptnames[3], ] <- c(thirdpointxy, 0)

  #fourth point
  fourthpointxyz <- pos4(cdmat[ptnames[1], ptnames[2]], 
                         cdmat[ptnames[1], ptnames[4]], 
                         cdmat[ptnames[2], ptnames[4]], 
                         cdmat[ptnames[3], ptnames[4]])[1, ]

  coords[ptnames[4], ] <- fourthpointxyz

  # subsequent points
  positions <- lapply(ptnames[-c(1:4)], function(x) 
      pos4(cdmat[ptnames[1], ptnames[2]],
           cdmat[ptnames[1], x],
           cdmat[ptnames[2], x],
           cdmat[ptnames[3], x])
      )
  names(positions) <- ptnames[-c(1:4)]


eucdis <- lapply(positions, function(x) dist(rbind(x, coords[4,]))[c(2,3)])

whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - cdmat[4, x])))
names(whichdist) <- names(eucdis)

coords[names(eucdis), ] <- do.call(rbind,
  lapply(names(eucdis), function(x) positions[[x]][whichdist[[x]], ]))
  
chromcoords <- as.data.frame(coords)

attr(chromcoords, 'class') <- c('colspace', 'data.frame')

chromcoords
}
