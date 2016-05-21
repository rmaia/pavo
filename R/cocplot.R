#' Plot the colour opponent coding diagram
#' 
#' \code{cocplot} produces a plot based on the colour opponent coding diagram of Backhaus (1991).
#' 
#' @usage plot(clrspdata)
#' 
#' @aliases plot.colspace
#'  
#' @param cocdata (required) a data frame, possibly a result from the \code{categorical} 
#'  function, containing values for 'x' and 'y' coordinates as columns (labeled as such)
#' @param labels plot axis labels? Defaults to \code{TRUE}
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?
#' @param achrosize size of the point at the origin when \code{achro = TRUE} (defaults to 0.8)
#' @param achrocol color of the point at the origin \code{achro = TRUE} (defaults to grey)
#' @param cex.labels character expansion factor for category labels when \code{labels = TRUE})
#' @param tick.loc a numeric vector specifying the location of tick marks on x & y axes 
#' @param ... additional graphical options. See \code{\link{par}}
#'    
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis', qcatch = 'Ei', relative = FALSE, vonkries = TRUE)
#' coc.flowers <- colspace(vis.flowers, space = 'coc')
#' plot(coc.flowers)
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @references Backhaus W. (1991). Color opponent coding in the visual system
#'  of the honeybee. Vision Research, 31, 1381-1397.

cocplot <- function(cocdata, labels = TRUE, cex.labels = 0.9, tick.loc = c(-12, -9, -6, -3, 3, 6, 9, 12), 
                     achro = FALSE, achrosize = 0.8, achrocol = 'grey', ...){ 
  
  arg <- list(...)
  
  # Set defaults
  if(is.null(arg$col))
    arg$col <- 'black'
  if(is.null(arg$pch))
    arg$pch <- 19  
  if(is.null(arg$xlim))
    arg$xlim  = c(-12, 12)
  if(is.null(arg$ylim))
    arg$ylim  = c(-12, 12)
  if(is.null(arg$xlab))
    arg$xlab = ' '
  if(is.null(arg$ylab))
    arg$ylab = ' '
  if(is.null(arg$bty))
    arg$bty = 'n'
  
  # Plot
  arg$x <- cocdata$x
  arg$y <- cocdata$y
  arg$axes = FALSE
  
  do.call(plot, arg)
  axis(1, at = tick.loc, pos = 0, cex.axis = 0.8)  # todo - best way to handle user specs?
  axis(2, at = tick.loc, pos = 0, cex.axis = 0.8, las = 2)
  
  # Origin point
  if(isTRUE(achro)){
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }
  
  # Category labels (todo: make this more flexible/robust?)
  if(labels == TRUE){
    text('B', x = 0, y = 13.5, xpd = TRUE, cex = cex.labels)
    text('A', x = 13.5, y = 0, xpd = TRUE, cex = cex.labels)
  }
  
}