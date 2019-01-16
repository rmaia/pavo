#' Plot the categorical colour vision model
#'
#' Produces a plot based on Troje's (1993) categorical colour model.
#'
# #' @usage plot(catdata, ...)
#'
#' @param catdata (required) a data frame, possibly a result from the \code{colspace}
#'  or \code{categorical} function, containing values for 'x' and 'y' coordinates
#'  as columns (labeled as such).
#' @param labels plot category labels inside? Defaults to \code{TRUE}.
#' @inheritParams triplot
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, qcatch = 'Qi', visual = 'musca', achro = 'none', relative = TRUE)
#' cat.flowers <- colspace(vis.flowers, space = 'categorical')
#' plot(cat.flowers)
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @keywords internal
#'
#' @inherit categorical references

catplot <- function(catdata, labels = TRUE, labels.cex = 0.9, ...) {

  # Check if object is of class colorspace and tetrachromat
  if (!is.colspace(catdata) & !all(c("x", "y") %in% names(catdata))) {
    stop("object is not of class ", dQuote("colspace"),
         ", and does not contain x, y coordinates")
  }

  if (is.colspace(catdata) & attr(catdata, "clrsp") != "categorical") {
    stop(dQuote("colspace"), " object is not a result of categorical()")
  }

  arg <- list(...)

  # Set defaults
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }
  if (is.null(arg$xlim)) {
    arg$xlim <- c(-1, 1)
  }
  if (is.null(arg$ylim)) {
    arg$ylim <- c(-1, 1)
  }
  if (is.null(arg$xlab)) {
    arg$xlab <- "R7p - R8p"
  }
  if (is.null(arg$ylab)) {
    arg$ylab <- "R7y - R8y"
  }

  # Plot
  arg$x <- catdata$x
  arg$y <- catdata$y

  do.call(plot, arg)
  abline(h = 0, v = 0, col = "grey") # Divide up categories

  # Category labels (todo: make this more flexible/robust?)
  if (labels) {
    legend(x = "topleft", legend = "p- y+", bty = "n", cex = labels.cex)
    legend(x = "topright", legend = "p+ y+", bty = "n", cex = labels.cex)
    legend(x = "bottomleft", legend = "p- y-", bty = "n", cex = labels.cex)
    legend(x = "bottomright", legend = "p+ y-", bty = "n", cex = labels.cex)
  }
}
