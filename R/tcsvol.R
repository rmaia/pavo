#' Plot a Tetrahedral Color Space
#'
#' \code{tcsvol} produces a 3D convex hull in tetrahedral color space
#'
#' @return \code{tcsvol} creates a 3D convex hull within a \code{tcsplot} object
#'
#' @rdname tcsplot
#' 

tcsvol <- function(tcsdata, static = TRUE, col = 'black', alpha = 0.2, 
                     grid.alpha = 1, grid = T, fill = T, lwd = 1){
  
  if(attr(tcsdata, 'clrsp') != 'tcs') stop("object is not in tetrahedral color space")
  
  
  vol <- t(convhulln(tcsdata[, c('x', 'y', 'z')], options = 'FA')$hull)
  coords <- tcsdata[, c('x', 'y', 'z')]
  listvol <- split(vol, rep(1:ncol(vol), each = nrow(vol)))
  ppairs <- do.call(rbind, lapply(listvol, function(x)t(combn(x, 2))))
  
  
  # STATIC
  if(static){
  last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
  
  flatcoords <- data.frame(last_tetraplot$xyz.convert(coords))
  
  if(is.character(col))
    col <- rgb(t(col2rgb(col)), alpha=grid.alpha*255, maxColorValue=255) 
  
  if(grid){
  for(i in 1:nrow(ppairs)){
  	segments(flatcoords[ppairs[i,1], 'x'], flatcoords[ppairs[i,1], 'y'],
  	         flatcoords[ppairs[i,2], 'x'], flatcoords[ppairs[i,2], 'y'],
  	         color=col, lwd=lwd) 
  }
  }
  
  if(fill){
    if(is.character(col))
      fillcol <- rgb(t(col2rgb(col)), alpha=alpha*255, maxColorValue=255) 

  	for(i in 1:ncol(vol)){
  		polygon(flatcoords[vol[,i],'x'], flatcoords[vol[,i],'y'], col=fillcol)
  	}
  }
  
  }
  
  
  # load RGL, and attempt install if not found
  #loadrgl()
  
  # INTERACTIVE
  if(!static){
  
  if(!isNamespaceLoaded("rgl"))
    requireNamespace("rgl")
  

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
}
