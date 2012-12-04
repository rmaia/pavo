#' Plot a Tetrahedral Color Space
#'
#' \code{ttvol} produces a 3D convex hull in tetrahedral color space
#'
#' @return \code{ttvol} creates a 3D convex hull within a \code{ttplot} object
#'
#' @rdname ttplot
#' 
#' @export

ttvol <- function(tcsdata, col='black', grid=T, fill=T){

# if(class(tcsdata)=='tcs'){
  # dat <- tcsdata$tcs  
  # }else{
    # dat <- tcsdata
    # }

vol <- t(convhulln(tcsdata[,c('x','y','z')],options='FA')$hull)
coords <- tcsdata[,c('x','y','z')]
listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
ppairs <- do.call(rbind,lapply(listvol,function(x)t(combn(x,2))))

if(grid==T){
  for(i in 1:nrow(ppairs)){
      segments3d(coords[ppairs[i,],'x'], 
                 coords[ppairs[i,],'y'],
                 coords[ppairs[i,],'z'], color=col)
  }
}

if(fill==T)
rgl.triangles(coords[vol,1],coords[vol,2],coords[vol,3], alpha=0.2, color=col)
material3d(alpha=1)
}
