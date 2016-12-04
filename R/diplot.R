#' Plot a dichromat segment
#' 
#' Produces a dichromat segment plot. 
#'
#' @param didata (required) a data frame, possibly a result from the \code{dispace} 
#'  function, containing values for the 'x' coordinates as a column (labeled as such).
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?
#' @param labels plot verticy labels? Defaults to \code{TRUE}.
#' @param lab.cex character expansion factor for category labels when \code{labels = TRUE}).
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to \code{0.8}).
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to \code{'grey'}).
#' @param out.lwd,out.lcol,out.lty graphical parameters for the segment.
#' @param margins margins for the plot.
#' @param square logical. Should the aspect ratio of the plot be held to 1:1? 
#' (defaults to \code{TRUE})
#' @param ... additional graphical options. See \code{\link{par}}.
#'
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- colspace(vis.flowers, space = 'di')
#' plot(di.flowers)
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

diplot <- function(didata, labels = TRUE, achro = TRUE, achrocol = 'grey', achrosize = 0.8, 
                    lab.cex = 1, out.lwd = 1, out.lcol = 'black', out.lty = 1, 
                    margins = c(1, 1, 2, 2), square = TRUE,...){ 
  
  par(mar=margins)
  
  if(square) 
    par(pty='s')
    
  arg <- list(...)
  
# Set defaults
  if(is.null(arg$pch))
    arg$pch <- "|"

# Verticy coordinates  
  vert <- data.frame(x = c(-1/sqrt(2), 1/sqrt(2)), y = c(0, 0))
  
# Blank plot w/ segment
  plot(0, type = 'n', xlim = c(-1/sqrt(2), 1/sqrt(2)), ylim = c(-0.5, 0.5), 
       bty = 'n', axes = FALSE, xlab = ' ', ylab = ' ')
  segments(vert$x[1], vert$y[1], vert$x[2], vert$y[2], lwd = out.lwd, lty = out.lty, col = out.lcol)
  
# Add points
  arg$x <- didata$x
  arg$y <- rep(0, length(didata$x))
  do.call(points, arg)
  
# Origin
  if(isTRUE(achro)){
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }
  
# Add text (coloured points better as in tcsplot?)
   if(isTRUE(labels)){
     text('S', x = -0.76, y = 0, xpd = TRUE, cex = lab.cex)
     text('L', x = 0.76, y = 0, xpd = TRUE, cex = lab.cex)
   }
  
}