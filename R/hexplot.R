#' Plot a colour hexagon 
#' 
#' Produces a colour hexagon plot. 
#' 
#' @param hexdata (required) a data frame, possibly a result from the \code{hexagon} 
#'  function, containing values for the 'x' and 'y' coordinates as columns (labeled as such)
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?
#' @param labels plot verticy labels? Defaults to \code{TRUE}.
#' @param sectors plot the bee-hue sector dividers? Options are:
#'    \itemize{ 
#'        \item \code{'none'}: No sectors (default)
#'        \item \code{'fine'}: 36 10-degree sectors
#'        \item \code{'coarse'}: six bee-hue sectors (blue, blue-green, green, uv-green, uv, uv-blue).
#'        }
#' @param lab.cex character expansion factor for category labels when \code{labels = TRUE})
#' @param sec.col line colour of hue sector dividers. Defaults to \code{'grey'}.
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to 0.8)
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to grey)
#' @param out.lwd,out.lcol,out.lty graphical parameters for hexagon outline.
#' @param margins margins for the plot.
#' @param square logical. Should the aspect ratio of the plot be held to 1:1? 
#' (defaults to \code{TRUE}).
#' @param ... additional graphical options. See \code{\link{par}}.
#'    
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, 
#'                         vonkries = TRUE, achro = 'l', bkg = 'green')
#' hex.flowers <- colspace(vis.flowers, space = 'hexagon')
#' plot(hex.flowers)
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'    based on photoreceptor excitations as a generalized representation of 
#'    colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Chittka L, Shmida A, Troje N, Menzel R. (1994). Ultraviolet as a 
#'    component of flower reflections, and the colour perception of Hymenoptera. 
#'    Vision research, 34(11), 1489-1508.

hexplot <- function(hexdata, achro = TRUE, labels = TRUE, 
                    sectors = c('none', 'fine', 'coarse'), sec.col = 'grey',
                    out.lwd = 1, out.lty = 1, out.lcol = 'black', 
                    lab.cex = 1, achrosize = 0.8,
                    achrocol = 'grey', margins = c(1, 1, 2, 2), square = TRUE,  ...){
  
  sectors <- match.arg(sectors)

  omargin <- par()$mar
  oAR <- par()$pty
  
  par(mar = margins)
  
  if(square) 
    par(pty = 's')
  
  on.exit(par(mar = omargin, pty = oAR))

# Set defaults
  arg <- list(...)
  
  if(is.null(arg$col))
    arg$col <- 'black'
  if(is.null(arg$pch))
    arg$pch <- 19
  if(is.null(arg$cex))
    arg$cex <- 0.9
#  if(is.null(arg$type)) 
#    arg$type = 'p'      is the default, not needed?
  if(is.null(arg$xlim))
    arg$xlim <- c(-1.2, 1.2)
  if(is.null(arg$ylim))
    arg$ylim <- c(-1.2, 1.2)

# Hexagon edge coordinates
  hexX <- c(0, -0.886, -0.886, 0, 0.886, 0.886, 0)
  hexY <- c(1, 0.5, -0.5, -1, -0.5, 0.5, 1)
  hexout <- data.frame(hexX, hexY)
  
# Hue sector divider coordinates
  # Coarse (45-degree)
  secX <- c(0.886, -0.886, 0.4429999, -0.4429999, 0.4429999, -0.4429999, 0.886)  
  secY <- c(0, 0, 0.75, -0.75, -0.75, 0.75, 0)
  
  # Fine (10-degree)
  secX_c <- c(0.886, 0.886, 0.886, 0.886, 0.7383333, 0.5906666, 0.4429999, 0.2953332, 0.1476665, 0,
              0.886, 0.886, 0.886, 0.886, 0.7383333, 0.5906666, 0.4429999, 0.2953332, 0.1476665, 0,
             -0.886, -0.886, -0.886, -0.886, -0.7383333, -0.5906666, -0.4429999, -0.2953332, -0.1476665, 0,
             -0.886, -0.886, -0.886, -0.886, -0.7383333, -0.5906666, -0.4429999, -0.2953332, -0.1476665, 0)  
  secY_c <- c(0, 0.1666667, 0.3333333, 0.5, 0.5833333, 0.6666667, 0.75, 0.8333333, 0.9166667, 1,
              0, -0.1666667, -0.3333333, -0.5, -0.5833333, -0.6666667, -0.75, -0.8333333, -0.9166667, -1,
              0, -0.1666667, -0.3333333, -0.5, -0.5833333, -0.6666667, -0.75, -0.8333333, -0.9166667, -1,
              0, 0.1666667, 0.3333333, 0.5, 0.5833333, 0.6666667, 0.75, 0.8333333, 0.9166667, 1)
  
# Plot
  arg$x <- hexdata$x
  arg$y <- hexdata$y
  arg$xlab = ''
  arg$ylab = ''
  arg$bty = 'n'
  arg$axes = FALSE
  
  do.call(plot, c(arg, type='n'))

# Origin point
  if(isTRUE(achro)){
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }
  
# Hexagon outline
  for(x in 1:length(hexX)){
    segments(hexX[x], hexY[x], hexX[x + 1], hexY[x + 1], lwd = out.lwd, col = out.lcol, lty = out.lty)      
  }
  
# Hexagon sectors
  if(sectors == 'coarse'){
    for(x in 1:length(secX))
      segments(0, 0, secX[x], secY[x], col = sec.col)
  }
  
  if(sectors == 'fine'){
    for(x in 1:length(secX_c))
      segments(0, 0, secX_c[x], secY_c[x], col = sec.col)
  }

# remove plot-specific args, add points after the stuff is drawn
arg[c('type', 'xlim', 'ylim', 'log', 'main', 'sub', 'xlab', 'ylab', 
'ann', 'axes', 'frame.plot', 'panel.first', 'panel.last', 'asp')] <- NULL
do.call(points, arg)
  
# Text labels
  if(isTRUE(labels)){
    text('E(B)', x = 0, y = 1.1, cex = lab.cex)
    text('E(UV)', x = -1, y = -0.6, cex = lab.cex)
    text('E(G)', x = 1, y = -0.6, cex = lab.cex)
  }
}