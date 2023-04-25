#' Plot the segment-analysis model
#'
#' Produces a plot based on Endler's (1990) segment analysis.
#'
#' @param segdata (required) a data frame, possibly a result from the
#'   [colspace()] or [segspace()] function, containing values for 'LM' and 'MS'
#'   as columns (labeled as such).
#' @inheritParams cocplot
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "segment", achromatic = "all")
#' seg.flowers <- colspace(vis.flowers, space = "segment")
#' plot(seg.flowers)
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @importFrom graphics axis par points polygon text
#'
#' @export
#'
#' @keywords internal
#'
#' @inherit segspace references

segplot <- function(segdata, labels = TRUE, lab.cex = 0.9,
                    out.lwd = 1, out.lty = 1, out.lcol = "black",
                    tick.loc = c(-1, -0.5, 0.5, 1),
                    square = TRUE, ...) {

  # Check if object is of class colorspace and tetrachromat
  if (!is.colspace(segdata) && !all(c("LM", "MS") %in% names(segdata))) {
    stop(
      "object is not of class ", dQuote("colspace"),
      ", and does not contain LM, MS segment data"
    )
  }

  if (is.colspace(segdata) && attr(segdata, "clrsp") != "segment") {
    stop(dQuote("colspace"), " object is not a result of segspace()")
  }

  arg <- list(...)

  # Set defaults
  if (square) {
    arg$asp <- 1
  }
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }
  if (is.null(arg$xlim)) {
    arg$xlim <- c(-1.05, 1.01)
  }
  if (is.null(arg$ylim)) {
    arg$ylim <- c(-1.05, 1.05)
  }
  if (is.null(arg$xlab)) {
    arg$xlab <- " "
  }
  if (is.null(arg$ylab)) {
    arg$ylab <- " "
  }
  arg$bty <- "n"
  arg$axes <- FALSE
  arg$type <- "n"

  # Plot
  arg$x <- segdata$MS
  arg$y <- segdata$LM

  do.call(plot, arg)
  axis(1, at = tick.loc, pos = 0, cex.axis = 0.8) # todo - best way to handle user specs?
  axis(2, at = tick.loc, pos = 0, cex.axis = 0.8, las = 2)

  # Segment edge coordinates
  segX <- c(-1, 0, 1, 0)
  segY <- c(0, -1, 0, 1)

  # Segplot outline1
  polygon(segX, segY, lwd = out.lwd, border = out.lcol, lty = out.lty)

  # Remove plot-specific args, add points after the stuff is drawn
  arg[c(
    "type", "xlim", "ylim", "log", "main", "sub", "xlab", "ylab",
    "ann", "axes", "frame.plot", "panel.first", "panel.last", "asp"
  )] <- NULL
  do.call(points, arg)

  # Category labels (todo: make this more flexible/robust?)
  if (labels) {
    text(
      x = segX,
      y = segY,
      labels = c("S1", "S2", "S3", "S4"),
      pos = c(2, 1, 4, 3),
      xpd = TRUE,
      cex = lab.cex
    )
  }
}
