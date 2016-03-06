#' CIE plot
#' 
#' Plot a CIE (XYZ or LAB) chromaticity diagram. 
#' 
#' @param ciedata (required)
#' @param mono should the monochromatic loci (the 'horseshoe') be
#'    plotted? Defaults to \code{TRUE}
#' @param out.lwd line width for monochromatic loci outline (defaults to 1)
#' @param out.lcol line colour for monochromatic loci outline (defaults to black)
#' @param out.lty line type for monochromatic loci outline (defaults to 1)
#' @param ... Additional graphical options. See \code{\link{par}}.
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @references Smith T, Guild J. (1932) The CIE colorimetric standards and their use.
#'    Transactions of the Optical Society, 33(3), 73-134.
#' @references Westland S, Ripamonti C, Cheung V. (2012). Computational colour science 
#'    using MATLAB. John Wiley & Sons.
#'    
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'cie2', illum = 'D65')
#' flowers.cie <- colspace(vis.flowers, space = 'ciexyz')
#' plot(flowers.cie)
#' }

.cieplot <- function(ciedata, mono = TRUE, out.lwd = NULL, out.lcol = 'black', 
                     out.lty = 1, ...){
  
# Check if object is of class colorspace and trichromat
  if(!('colspace' %in% attr(ciedata, 'class')) & is.element(FALSE, c('x', 'y') %in% names(ciedata)))
    stop('object is not of class ', dQuote('colspace'), ', and does not contain x, y coordinates')
  
  if(('colspace' %in% attr(ciedata, 'class')) & !grepl('CIE', attr(ciedata, 'clrsp')))
    stop(dQuote('colspace'), ' object is not a result of ', dQuote('cie()'))
  
  arg <- list(...)
  
  # CIEXYZ
  if(attr(ciedata, 'clrsp') == 'CIEXYZ'){
    
    # Set defaults
    if(is.null(arg$col))
      arg$col <- 'black'
    if(is.null(arg$pch))
      arg$pch <- 19
    if(is.null(arg$type))
      arg$type = 'p'
    if(is.null(arg$xaxp))
      arg$xaxp <- c(0, 0.9, 9)
    if(is.null(arg$yaxp))
      arg$yaxp <- c(0, 0.8, 8)
    if(is.null(arg$xlim))
      arg$xlim <- c(0, 0.75)
    if(is.null(arg$ylim))
      arg$ylim <- c(0, 0.85)
    if(is.null(arg$xlab))
      arg$xlab <- 'x'
    if(is.null(arg$ylab))
      arg$ylab <- 'y'
    
    # Monochromatic loci in XYZ, from Westland et al. 2012
    monox <- c(0.175596, 0.172787, 0.170806, 0.170085, 0.160343, 0.146958, 0.139149,
               0.133536, 0.126688, 0.115830, 0.109616, 0.099146, 0.091310, 0.078130,
               0.068717, 0.054675, 0.040763, 0.027497, 0.016270, 0.008169, 0.004876,
               0.003983, 0.003859, 0.004646, 0.007988, 0.013870, 0.022244, 0.027273,
               0.032820, 0.038851, 0.045327, 0.052175, 0.059323, 0.066713, 0.074299,
               0.089937, 0.114155, 0.138695, 0.154714, 0.192865, 0.229607, 0.265760,
               0.301588, 0.337346, 0.373083, 0.408717, 0.444043, 0.478755, 0.512467,
               0.544767, 0.575132, 0.602914, 0.627018, 0.648215, 0.665746, 0.680061,
               0.691487, 0.700589, 0.707901, 0.714015, 0.719017, 0.723016, 0.734674)
    monoy <- c(0.005295, 0.004800, 0.005472, 0.005976, 0.014496, 0.026643, 0.035211,
               0.042704, 0.053441, 0.073601, 0.086866, 0.112037, 0.132737, 0.170464,
               0.200773, 0.254155, 0.317049, 0.387997, 0.463035, 0.538504, 0.587196,
               0.610526, 0.654897, 0.675970, 0.715407, 0.750246, 0.779682, 0.792153,
               0.802971, 0.812059, 0.819430, 0.825200, 0.829460, 0.832306, 0.833833,
               0.833316, 0.826231, 0.814796, 0.805884, 0.781648, 0.754347, 0.724342,
               0.692326, 0.658867, 0.624470, 0.589626, 0.554734, 0.520222, 0.486611,
               0.454454, 0.424252, 0.396516, 0.372510, 0.351413, 0.334028, 0.319765,
               0.308359, 0.299317, 0.292044, 0.285945, 0.280951, 0.276964, 0.265326)
   
    # Plot
    arg$x <- ciedata$x
    arg$y <- ciedata$y
    
    do.call(plot, arg)
      
      if(mono == TRUE)
        polygon(monoy ~ monox, border = out.lcol, lty = out.lty, density = out.lwd)
    
  }
  
  # CIELAB
  if(attr(ciedata, 'clrsp') == 'CIELAB'){
    print('Nothing available yet')
  }
      
}