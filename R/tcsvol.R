#' Plot a tetrahedral color space
#'
#' Produces a 3D convex hull in tetrahedral color space
#'
#' @return \code{tcsvol} creates a 3D convex hull within a \code{tcsplot} object.
#'
#' @rdname tcsplot
#' @export
#' 

tcsvol <- function(tcsdata, col = 'black', alpha = 0.2, 
                     grid.alpha = 1, grid = TRUE, fill = TRUE, lwd = 1){
  
  if(attr(tcsdata, 'clrsp') != 'tcs') stop("object is not in tetrahedral color space")

  # check if rgl is installed and loaded
  if (!requireNamespace("rgl", quietly = TRUE))
    stop(dQuote('rgl'),' package needed for this function to work. Please install it.',
      call. = FALSE)  

  if(!isNamespaceLoaded("rgl"))
    requireNamespace("rgl")  
  
  vol <- t(convhulln(tcsdata[, c('x', 'y', 'z')], options = 'FA')$hull)
  coords <- tcsdata[, c('x', 'y', 'z')]
  listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
  ppairs <- do.call(rbind, lapply(listvol, function(x)t(combn(x, 2))))

  if(grid){
    for(i in 1:nrow(ppairs)){
        rgl::segments3d(coords[ppairs[i,], 'x'], 
                        coords[ppairs[i,], 'y'],
                        coords[ppairs[i,], 'z'], 
                        color = col, alpha = grid.alpha, lwd=lwd)
    }
  }
  
  if(fill)
  rgl::rgl.triangles(coords[vol, 1], coords[vol, 2], coords[vol, 3], 
    alpha = alpha, color = col)
    
  rgl::material3d(alpha = 1)
}
