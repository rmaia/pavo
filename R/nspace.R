#' n-chromatic colour space
#'
#' Calculates coordinates and colourimetric variables that represent reflectance
#' spectra in a n-chromatic colour space.
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the
#'   result from [vismodel()] or independently calculated data
#'   
#' @export
#' 
#' @importFrom geometry bary2cart
#'   
#' @example 
#' fakemantisshrimp <- sensmodel(c(325, 350, 400, 425, 450, 500, 550, 600, 650, 700), beta = FALSE, integrate = FALSE)
#' 
#' data(flowers)
#' vis_flowers <- vismodel(flowers, visual = fakemantisshrimp)
#' colsp_flowers <- nspace(vis_flowers)

nspace <- function(vismodeldata) {
  
  qcatches <- vismodeldata[, colnames(vismodeldata) != "lum"]
  ncones <-ncol(qcatches)
  
  # Get relative qcatches
  qcatches <- qcatches / rowSums(qcatches)
  
  coords <- bary2cart(simplex(ncones), as.matrix(qcatches))
  
  r.vec <- sqrt(rowSums(apply(coords, 2, function(x) x^2)))
  
  return(data.frame(qcatches, coords, r.vec))
}

simplex <- function(n) {
  qr.Q(qr(matrix(1,nrow=n)),complete=T)[,-1]
}