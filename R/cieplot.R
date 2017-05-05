#' CIE plot
#' 
#' Plot a CIE (XYZ or LAB) chromaticity diagram. 
#' 
#' @import scatterplot3d 
#' 
# #' @usage plot(ciedata, ...)
#' 
#' @param ciedata (required). a data frame, possibly a result from the \code{colspace} 
#' or \code{cie} function, containing values for 'x', 'y' and 'z' coordinates 
#' as columns (labeled as such).
#' @param mono should the monochromatic loci (the 'horseshoe') be
#'    plotted? Defaults to \code{TRUE}.
#' @param out.lwd,out.lcol,out.lty graphical parameters for the monochromatic loci outline. 
#' @param view orientation of the 3d plot in degrees, when \code{space = 'cielab'} (defaults to 70).
#' @param scale.y numeric. Perspective scaling of the y axis (defaults to \code{0.45}).
#' @param axis logical. Draw X, Y and Z axis (defaults to \code{FALSE}).
#' @param grid logical. Draw grid (defaults to \code{FALSE}).
#' @param ... Additional graphical options. See \code{\link{par}}.
#' @param xlim,ylim,zlim axis limits
#' @param margin vector of four numbers specifying drawing margins (defaults to c(1,1,1,1))
#' 
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'cie2', illum = 'D65')
#' cie.flowers <- colspace(vis.flowers, space = 'ciexyz')
#' plot(cie.flowers)
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @references Smith T, Guild J. (1932) The CIE colorimetric standards and their use.
#'    Transactions of the Optical Society, 33(3), 73-134.
#' @references Westland S, Ripamonti C, Cheung V. (2012). Computational colour science 
#'    using MATLAB. John Wiley & Sons.
#'    


cieplot <- function(ciedata, mono = TRUE, out.lwd = NULL, out.lcol = 'black', 
                     out.lty = 1, view = 70, scale.y = 0.45, axis = FALSE, grid = FALSE,
                     xlim = c(-12.8, 12.7), ylim = c(-12.8, 12.7), 
                     zlim = c(0, 10.0), margin = c(1, 1, 1, 1),
                      ...){
  
  arg <- list(...)
  
  # CIEXYZ
  if(attr(ciedata, 'clrsp') == 'CIEXYZ'){
    
    # Set defaults
    if(is.null(arg$pch))
      arg$pch <- 19
    if(is.null(arg$xaxp))
      arg$xaxp <- c(0, 0.9, 9)
    if(is.null(arg$yaxp))
      arg$yaxp <- c(0, 0.8, 8)
    if(is.null(arg$xlim))
      arg$xlim <- c(0, 0.75)
    if(is.null(arg$ylim))
      arg$ylim <- c(0, 0.85)
    if(is.null(arg$xlab))
      arg$xlab <- 'CIE x'
    if(is.null(arg$ylab))
      arg$ylab <- 'CIE y'
    
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
    
    do.call(plot, c(arg, type='n'))
      
    if(mono == TRUE)
      polygon(monoy ~ monox, border = out.lcol, lty = out.lty, density = out.lwd)
    
    # remove plot-specific args, add points after the stuff is drawn
    arg[c('type', 'xlim', 'ylim', 'log', 'main', 'sub', 'xlab', 'ylab', 
    'ann', 'axes', 'frame.plot', 'panel.first', 'panel.last', 'asp')] <- NULL
    do.call(points, arg)

    
  }
  
  # CIELAB
  if(attr(ciedata, 'clrsp') == 'CIELAB'){
    
    # Set defaults
    arg <- list(...)
    
    if(is.null(arg$cex))
      arg$cex <- 0.9
    if(is.null(arg$pch))
      arg$pch <- 19

    P <- scatterplot3d(mean(xlim), mean(ylim), mean(zlim), box = TRUE, 
                          xlim = xlim, ylim = ylim, 
                          zlim = zlim, axis = axis, grid = grid, angle = view, 
                          scale.y = scale.y, mar = margin, pch = '')
    
    # LAB plot axis line vertices
    L1 <- P$xyz.convert(0, 0, 0)
    L2 <- P$xyz.convert(0, 0, 10.0)
    a1 <- P$xyz.convert(-12.8, 0, 5.0)
    a2 <- P$xyz.convert(12.7, 0, 5.0)
    b1 <- P$xyz.convert(0, -12.8, 5.0)
    b2 <- P$xyz.convert(0, 12.7, 5.0)
    
    # Text label locations
    txt_L <- P$xyz.convert(0, 0, 10.4)
    txt_a <- P$xyz.convert(-14.0, 0, 5.0)
    txt_b <- P$xyz.convert(0, -15.0, 5.0)
  
    # Draw them up
    segments(L1$x, L1$y, L2$x, L2$y, lwd = 1.5)
    segments(a1$x, a1$y, a2$x, a2$y, lwd = 1.5)
    segments(b1$x, b1$y, b2$x, b2$y, lwd = 1.5)
    
    # Axis labels
    text(x = txt_L$x, y = txt_L$y, labels = "L")
    text(x = txt_a$x, y = txt_a$y, labels = "a")
    text(x = txt_b$x, y = txt_b$y, labels = "b")
    
    # Data
    arg$x <- ciedata$a
    arg$y <- ciedata$b
    arg$z <- ciedata$L
    
    do.call(P$points3d, arg)
    
    # Save plot info 
    #.PlotCielabEnv <<- new.env()
    assign("last_plot.cielab", P, envir = .PlotCielabEnv)
  }
      
}