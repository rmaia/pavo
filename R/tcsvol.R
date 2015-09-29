#' Plot a Tetrahedral Color Space
#'
#' \code{tcsvol} produces a 3D convex hull in tetrahedral color space
#'
#' @return \code{tcsvol} creates a 3D convex hull within a \code{tcsplot} object
#'
#' @rdname tcsplot
#' 
#' @export

tcsvol <- function(tcsdata, col='black', alpha=0.2, grid.alpha=1, grid=T, fill=T){

# load RGL, and attempt install if not found
loadrgl()

vol <- t(convhulln(tcsdata[,c('x','y','z')],options='FA')$hull)
coords <- tcsdata[,c('x','y','z')]
listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
ppairs <- do.call(rbind,lapply(listvol,function(x)t(combn(x,2))))

if(grid==T){
  for(i in 1:nrow(ppairs)){
      segments3d(coords[ppairs[i,],'x'], 
                 coords[ppairs[i,],'y'],
                 coords[ppairs[i,],'z'], color=col, alpha=grid.alpha)
  }
}

if(fill==T)
rgl.triangles(coords[vol,1],coords[vol,2],coords[vol,3], alpha=alpha, color=col)
material3d(alpha=1)
}
