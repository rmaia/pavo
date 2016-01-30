#' Plot spectra in various colourspaces
#'
#' yup
#' 
#' @details details yo
#' 
#' @usage usage 
#' 
#' @param x (required) a data frame, possibly an object of class \code{colorspace}, 
#' blah
#' @param foo
#' \itemize{
#'  \item \code{stuff} 
#' }
#' @param sectors plot the bee-hue sector dividers? Options are:
#'    \itemize{ 
#'        \item \code{'none'}: No sectors (default)
#'        \item \code{'fine'}: 36 10-degree sectors
#'        \item \code{'coarse'}: six bee-hue sectors (blue, blue-green, green, uv-green, uv, uv-blue).
#'        }
#'        
#' @param ... additional arguments passed to plot.
#' 
#' @note Some obly available blah:
#' 
#' @param Hoopah
#' 
#' @section Things section
#' 
#' 
#' @export
#' 
#' @examples \dontrun{
#' 
#' }
#' 
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
  
  if(attr(clrspdata, 'clrsp') == 'tcs'){
    .tcsplot(clrspdata, ...)
  }
  
}