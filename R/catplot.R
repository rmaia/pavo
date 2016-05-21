#' Plot the categorical colour vision model
#' 
#' \code{catplot} produces a plot based on Troje's (1993) categorical colour model. 
#' Note that the model is 'categorical' and results were not intended to be 
#' interpreted as continuous, which may be implied by this type of plot
#' 
#' @usage \code{plot(clrspdata)}.
#'  
#' @param catdata (required) a data frame, possibly a result from the \code{categorical} 
#'  function, containing values for 'x' and 'y' coordinates as columns (labeled as such)
#' @param labels plot category labels inside? Defaults to \code{TRUE}.
#' @param cex.labels character expansion factor for category labels when \code{labels = TRUE}).
#' @param ... additional graphical options. See \code{\link{par}}
#'    
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'bluetit', relative = TRUE, scale = 10000)
#' cat.flowers <- colspace(vis.flowers, space = 'categorical')
#' plot(cat.flowers)
#' }
#' 
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @references Troje N. (1993). Spectral categories in the learning behaviour
#'  of blowflies. Zeitschrift fur Naturforschung C, 48, 96-96.

catplot <- function(catdata, labels = TRUE, cex.labels = 0.9, ...){ 
  
# Check if object is of class colorspace and tetrachromat
  if(!('colspace' %in% attr(catdata, 'class')) & is.element(FALSE, c('x', 'y') %in% names(catdata)))
    stop('object is not of class ', dQuote('colspace'), ', and does not contain x, y coordinates')
  
  if(('colspace' %in% attr(catdata, 'class')) & attr(catdata, 'clrsp') != 'categorical')
    stop(dQuote('colspace'), ' object is not a result of categorical()')
  
  arg <- list(...)
  
# Set defaults
  if(is.null(arg$col))
    arg$col <- 'black'
  if(is.null(arg$pch))
    arg$pch <- 19  
  if(is.null(arg$xlim))
    arg$xlim  = c(-1, 1)
  if(is.null(arg$ylim))
    arg$ylim  = c(-1, 1)
  if(is.null(arg$xlab))
    arg$xlab = 'R7p - R8p'
  if(is.null(arg$ylab))
    arg$ylab = 'R7y - R8y'
    
# Plot
  arg$x <- catdata$x
  arg$y <- catdata$y
  
  do.call(plot, arg)
  abline(h = 0, v = 0, col = 'grey')  # Divide up categories
  
# Category labels (todo: make this more flexible/robust?)
  if(labels == TRUE){
    legend(x = 'topleft', legend = 'p- y+', bty = 'n', cex = cex.labels)
    legend(x = 'topright', legend = 'p+ y+', bty = 'n', cex = cex.labels)
    legend(x = 'bottomleft', legend = 'p- y-', bty = 'n', cex = cex.labels)
    legend(x = 'bottomright', legend = 'p+ y-', bty = 'n', cex = cex.labels)
  }

}