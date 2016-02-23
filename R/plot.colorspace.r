#' Plot spectra in a colourspace
#'
#' \code{plot.colorspace} Plots reflectance spectra in the appropriate colorspace
#' 
#' @param clrspdata (required) an object of class \code{colorspace}. May be a result from
#'  \code{dispace}, \code{maxwell}, \code{coc}, \code{hexagon}, \code{categorical}, 
#'  \code{tcs}, or \code{cie}. Note that the below options will vary by plot type. 
#' 
#' @param labels plot verticy labels (or category labels, when clrspdata is the result of
#'  \code{categorical})? Defaults to \code{TRUE}
#' @param cex.labels character expansion factor for labels when \code{labels = TRUE})
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})? Has
#'  no effect when clrspdata is the result of \code{categorical}, \code{coc}, or \code{cie}
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to 0.8)
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to grey)
#' @param out.lwd line width for plot outline, or the width of the monochromatic loci
#'  outline in CIEXYZ space when \code{mono = TRUE} (defaults to 1). Has no effect when
#'  clrspdata is the result of \code{coc} 
#' @param out.lcol line colour for plot outline, or the colour of the monochromatic loci
#'  outline in CIEXYZ space when \code{mono = TRUE} (defaults to black). Has no effect when
#'  clrspdata is the result of \code{coc}
#' @param out.lty line type for plot outline, or the line type for the monochromatic loci
#'  outline in CIEXYZ space when \code{mono = TRUE} (defaults to 1). Has no effect when
#'  clrspdata is the result of \code{coc}
#' @param tick.loc a numeric vector specifying the location of tick marks on x & y axes,
#'  only when clrspdata is the result of \code{dispace}
#' @param mono plot the monochromatic loci (the 'horseshoe') in CIEXYZ space? 
#'  Defaults to \code{TRUE}.   
#' @param sectors plot the bee-hue sector dividers (only when clrspdata is the result 
#' of \code{hexagon})? Options are:
#'    \itemize{ 
#'        \item \code{'none'}: No sectors (default)
#'        \item \code{'fine'}: 36 10-degree sectors
#'        \item \code{'coarse'}: six bee-hue sectors (blue, blue-green, green, uv-green, uv, uv-blue).
#'        }
#' @param col.sec line colour of bee-hue sector dividers. Defaults to \code{'grey'}
#' @param ... additional graphical options. See code{\link{par}} 
#' 
#' @examples \dontrun{
#' data(flowers)
#' 
#' # Dichromat space
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- dispace(vis.flowers)
#' plot(di.flowers)
#'
#' # Colour hexagon 
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- hexagon(vis.flowers)
#' plot(hex.flowers)
#' 
#' # Maxwell triangle
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' maxwell.flowers <- maxwell(vis.flowers)
#' plot(maxwell.flowers)
#' }
#'  
#' @seealso \code{\link{plot}}

plot.colorspace <- function(clrspdata, ...) {
  
  # Check if object is of class colorspace. TODO: accept non-colorspace objects?
  if(!('colorspace' %in% attr(clrspdata, 'class')))
    stop('object is not of class ', dQuote('colorspace'))
  
  if(attr(clrspdata, 'clrsp') == 'hexagon'){
    .hexplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'coc'){
    .cocplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'dispace'){
    .diplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'maxwell'){
    .maxplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'CIEXYZ'){
    .cieplot(clrspdata, ...)
  }
  
  if(attr(clrspdata, 'clrsp') == 'tcs'){
    .tcsplot(clrspdata, ...)
  }
  
}