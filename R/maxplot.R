#' Plot a Maxwell Triangle
#' 
#' \code{maxplot} produces a Maxwell triangle plot
#'
#' @param maxdata (required) a data frame, possibly a result from the \code{maxwell} 
#' function, containing values for the 'x' and 'y' coordinates as columns (labeled as such)
#' @param achro should a point be plotted at the origin (defaults to \code{TRUE})?
#' @param achrosize size of the point at the origin (defaults to 0.8)
#' @param achrocol color of the point at the origin (defaults to grey)
#' @param out.lwd line width for triangle outline (defaults to 1)
#' @param out.lcol line colour for triangle outline (defaults to black)
#' @param out.lty line type for triangle outline (defaults to 1)
#' @param labsize size of the text labels (defaults to 1)
#' @param ... additional graphical options. See code{\link{par}}.
#'
#' @export
#'
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' maxwell.flowers <- maxwell(vis.flowers)
#' maxplot(maxwell.flowers)
#' }
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' 
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision
#'    - behavioural tests and physiological concepts. Biological Reviews, 78,
#'    81 - 118.
#' @references Neumeyer C (1980) Simultaneous color contrast in the honeybee. 
#' Journal of comparative physiology, 139(3), 165-176.

maxplot <- function(maxdata, achro = TRUE, achrocol = 'grey', achrosize = 0.8, labsize = 1, 
                    out.lwd = 1, out.lcol = 'black', out.lty = 1, ...){ 
  
# todo: 
#  is the triangle definitely the right dimensions?
#  better handling of user defined args for triangle outline 
  
# Check if object is of class colorspace and trichromat
if(!('colorspace' %in% attr(maxdata, 'class')))
  stop('object is not of class ', dQuote('colorspace'))

if(('colorspace' %in% attr(maxdata, 'class')) & attr(maxdata, 'conenumb') != 3)
  stop(dQuote('colorspace'), ' object is not a trichromat visual system')  

arg <- list(...)

# Set defaults
if (is.null(arg$col))
  arg$col <- 'black'
if (is.null(arg$pch))
  arg$pch <- 19
  
# Verticy coordinates  
vert <- data.frame(x = c(0, -1/sqrt(2), 1/sqrt(2)),
                     y = c(sqrt(2)/sqrt(3), -sqrt(2)/(2*(sqrt(3))), -sqrt(2)/(2*(sqrt(3)))))

# Plots vertices
plot(vert, type = 'n', xlab = " ", ylab = " ", bty = "n", axes = FALSE)

# Add lines 
segments(vert$x[1], vert$y[1], vert$x[2], vert$y[2], lwd = out.lwd, lty = out.lty, col = out.lcol)
segments(vert$x[1], vert$y[1], vert$x[3], vert$y[3], lwd = out.lwd, lty = out.lty, col = out.lcol)
segments(vert$x[2], vert$y[2], vert$x[3], vert$y[3], lwd = out.lwd, lty = out.lty, col = out.lcol)

# Origin
if (achro == TRUE){
  points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
}

# Add text (coloured points better as in tcsplot?)
text('S', x = -0.76, y = -0.39, xpd = TRUE, cex = labsize)
text('M', x = 0, y = 0.88, xpd = TRUE, cex = labsize)
text('L', x = 0.76, y = -0.39, xpd = TRUE, cex = labsize)

# Plot stimulus points
arg$x <- maxdata$x
arg$y <- maxdata$y
do.call(points, arg)
  
}