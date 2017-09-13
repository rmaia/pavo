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
#' @references Pike, T.W. (2012). Preserving perceptual distances in chromaticity diagrams. 
#' Behavioral Ecology, 23, 723–728.


jnd2xyz <- function(coldistres) {
 
   # Accessory functions
  pos2 <- function(d12, d13, d23){
	x3 <- d13
	
	if(!d23 < d12+d13)
	  x3 <- x3*-1
	  
	x3
  }

 
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
  
  ncone <- attr(coldistres,'ncone')

  if(as.numeric(ncone) < 2 || as.numeric(ncone) > 4 )
    stop('only methods for di-, tri- and tetrachromatic models are implemented so far', call.=TRUE)
  
  references <- attr(coldistres, 'resref')
  references <- references[intersect(
    grep('jnd2xyzrrf', references$patch1, invert=T), 
    grep('jnd2xyzrrf', references$patch2)
    ),]
    
  combined <- rbind(coldistres, references)
  
  colmat <- coldist2mat(combined)

  cdmat <- colmat[['dS']]

  coords <- matrix(NA, nrow=nrow(cdmat), ncol=as.numeric(ncone)-1, 
    dimnames=list(row.names(cdmat), c('x','y', 'z')[seq(as.numeric(ncone)-1)]))
  
  ptnames <- rownames(coords)
  
  if(ncone == '2'){
  # 2 cones, only 1 dimension
   
  # first point
  coords[ptnames[1], ] <- 0
  
  # second point
  coords[ptnames[2], ] <- cdmat[ptnames[1], ptnames[2]]
  
  # subsequent points  
  coords[c(ptnames[-c(1:2)]), ] <- do.call(rbind, lapply(ptnames[-c(1:2)], function(x)
                      pos2(cdmat[ptnames[1],ptnames[2]],
                           cdmat[ptnames[1],x],
                           cdmat[ptnames[2],x])
                      ))
                      
  }
  
  if(ncone == '3'){

  # first point
  coords[ptnames[1], ] <- c(0,0)
  
  # second point
  coords[ptnames[2], ] <- c(cdmat[ptnames[1], ptnames[2]],0)

  # third point
  coords[ptnames[3], ] <- pos3(cdmat[ptnames[1], ptnames[2]], 
                       cdmat[ptnames[1], ptnames[3]], 
                       cdmat[ptnames[2], ptnames[3]])[1, ]

  
  # subsequent points
  positions <- lapply(ptnames[-c(1:3)], function(x) 
      pos3(cdmat[ptnames[1], ptnames[2]],
           cdmat[ptnames[1], x],
           cdmat[ptnames[2], x])
      )
  names(positions) <- ptnames[-c(1:3)]


eucdis <- lapply(positions, function(x) dist(rbind(x, coords[ptnames[3],]))[c(2,3)])

whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - cdmat[ptnames[3], x])))
names(whichdist) <- names(eucdis)

coords[names(eucdis), ] <- do.call(rbind,
  lapply(names(eucdis), function(x) positions[[x]][whichdist[[x]], ]))  	
  }

  if(ncone == '4'){
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

whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - cdmat[ptnames[4], x])))
names(whichdist) <- names(eucdis)

coords[names(eucdis), ] <- do.call(rbind,
  lapply(names(eucdis), function(x) positions[[x]][whichdist[[x]], ]))
  
}

if('dL' %in% names(colmat)){
  ldmat <- colmat[['dL']]
  coords <- cbind(coords, lum=0)
  
  # first point
  coords[ptnames[1], 'lum'] <- 0
  
  # second point
  coords[ptnames[2], 'lum'] <- ldmat[ptnames[1], ptnames[2]]
  
  # subsequent points  
  coords[c(ptnames[-c(1:2)]), 'lum' ] <- do.call(rbind, lapply(ptnames[-c(1:2)], function(x)
                      pos2(ldmat[ptnames[1],ptnames[2]],
                           ldmat[ptnames[1],x],
                           ldmat[ptnames[2],x])
                      ))
  # invert if darkest point is positive
  
  
  # center on achromatic point
  coords[,'lum'] <- coords[,'lum'] - coords['jnd2xyzrrf.achro','lum']
  #coords[,'lum'] <- coords[,'lum'] - coords[darker,'lum']
}

jnd2xyzrrf.ctrd <- colMeans(coords[grep('jnd2xyzrrf', rownames(coords), invert=TRUE), , drop=FALSE])
  
chromcoords <- as.data.frame(coords[grep('jnd2xyzrrf', rownames(coords), invert=TRUE), , drop=FALSE])

refstosave <- as.data.frame(rbind(
  coords[grep('jnd2xyzrrf', rownames(coords)), , drop=FALSE], jnd2xyzrrf.ctrd))

attr(chromcoords, 'class') <- c('colspace', 'jnd2xyz', 'data.frame')
attr(chromcoords, 'resref') <- refstosave

chromcoords
}
