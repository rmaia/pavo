#' Plot a dichromat segment
#' 
#' \code{diplot} produces a dichromat segment plot. 
#' 
#' @usage \code{plot(clrspdata)}.
#'
#' @param didata (required) a data frame, possibly a result from the \code{dispace} 
#'  function, containing values for the 'x' coordinates as a column (labeled as such)
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?
#' @param labels plot verticy labels? Defaults to \code{TRUE}
#' @param cex.labels character expansion factor for category labels when \code{labels = TRUE})
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to 0.8)
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to grey)
#' @param out.lwd line width for segment (defaults to 1)
#' @param out.lcol line colour for segment (defaults to black)
#' @param out.lty line type for segment (defaults to 1)
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
                    cex.labels = 1, out.lwd = 1, out.lcol = 'black', out.lty = 1, ...){ 
  
  arg <- list(...)
  
# Set defaults
  if(is.null(arg$col))
    arg$col <- 'black'
  if(is.null(arg$pch))
    arg$pch <- 19
  if(is.null(arg$type))
    arg$type = 'p'
  if(is.null(arg$xlim))
    arg$xlim <- c(-1/sqrt(2), 1/sqrt(2))
  if(is.null(arg$ylim))
    arg$ylim <- c(-0.5, 0.5)
    
# Verticy coordinates  
  vert <- data.frame(x = c(-1/sqrt(2), 1/sqrt(2)), y = c(0, 0))
  
# Plot
  arg$x <- didata$x
  arg$y <- rep(0, length(didata$x))
  arg$xlab = ' '
  arg$ylab = ' '
  arg$bty = 'n'
  arg$axes = FALSE
  
  do.call(plot, arg)
  
# Add lines 
  segments(vert$x[1], vert$y[1], vert$x[2], vert$y[2], lwd = out.lwd, lty = out.lty, col = out.lcol)
  
# Origin
  if(isTRUE(achro)){
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }
  
# Add text (coloured points better as in tcsplot?)
   if(isTRUE(labels)){
     text('S', x = -0.76, y = 0, xpd = TRUE, cex = cex.labels)
     text('L', x = 0.76, y = 0, xpd = TRUE, cex = cex.labels)
   }
  
}