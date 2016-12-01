#' Plot a maxwell triangle
#' 
#' Produces a Maxwell triangle plot.
#'
#' @param tridata (required) a data frame, possibly a result from the \code{trispace} .
#'  function, containing values for the 'x' and 'y' coordinates as columns (labeled as such).
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?.
#' @param labels plot verticy labels? Defaults to \code{TRUE}.
#' @param cex.labels character expansion factor for category labels when \code{labels = TRUE}).
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to 0.8).
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to \code{'grey'}).
#' @param out.lwd,out.lcol,out.lty graphical parameters for the plot outline.
#' @param margins margins for the plot.
#' @param square logical. Should the aspect ratio of the plot be held to 1:1? 
#' (defaults to \code{TRUE})
#' @param ... additional graphical options. See \code{\link{par}}.
#'
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' tri.flowers <- colspace(vis.flowers, space = 'tri')
#' plot(tri.flowers)
#' }
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision
#'    - behavioural tests and physiological concepts. Biological Reviews, 78,
#'    81 - 118.
#' @references Neumeyer C (1980) Simultaneous color contrast in the honeybee. 
#'  Journal of comparative physiology, 139(3), 165-176.

triplot <- function(tridata, labels = TRUE, achro = TRUE, achrocol = 'grey', achrosize = 0.8, 
                     cex.labels = 1, out.lwd = 1, out.lcol = 'black', out.lty = 1, 
                     margins = c(1,1,2,2), square=TRUE, ...){ 

  omargin <- par()$mar
  oAR <- par()$pty
  
  par(mar=margins)
  
  if(square) 
    par(pty='s')
  
  on.exit(par(mar=omargin, pty=oAR))
  
  arg <- list(...)
  
# Set defaults
  if(is.null(arg$col))
    arg$col <- 'black'
  if(is.null(arg$pch))
    arg$pch <- 19
#  if(is.null(arg$type))
#    arg$type = 'p'      is the default, not needed?
  if(is.null(arg$xlim))
    arg$xlim <- c(-1/sqrt(2), 1/sqrt(2))
  if(is.null(arg$ylim))
    arg$ylim <- c(-sqrt(2)/(2*(sqrt(3))), sqrt(2)/sqrt(3))
    
# Verticy coordinates  
  vert <- data.frame(x = c(0, -1/sqrt(2), 1/sqrt(2)),
                       y = c(sqrt(2)/sqrt(3), -sqrt(2)/(2*(sqrt(3))), -sqrt(2)/(2*(sqrt(3)))))
  
# Plot
  arg$x <- tridata$x
  arg$y <- tridata$y
  arg$xlab = ''
  arg$ylab = ''
  arg$bty = 'n'
  arg$axes = FALSE
  
  do.call(plot, c(arg, type='n'))
  
# Add lines 
  segments(vert$x[1], vert$y[1], vert$x[2], vert$y[2], lwd = out.lwd, lty = out.lty, col = out.lcol)
  segments(vert$x[1], vert$y[1], vert$x[3], vert$y[3], lwd = out.lwd, lty = out.lty, col = out.lcol)
  segments(vert$x[2], vert$y[2], vert$x[3], vert$y[3], lwd = out.lwd, lty = out.lty, col = out.lcol)
  
# Origin
  if(isTRUE(achro)){
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }

# add points after the hexagon stuff is drawn
  suppressWarnings(do.call(points, arg))

  
# Add text (coloured points better as in tcsplot?)
  if(isTRUE(labels)){
    text('S', x = -0.76, y = -0.39, xpd = TRUE, cex = cex.labels)
    text('M', x = 0, y = 0.88, xpd = TRUE, cex = cex.labels)
    text('L', x = 0.76, y = -0.39, xpd = TRUE, cex = cex.labels)
  }
  
}