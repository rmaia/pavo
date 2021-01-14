#' Plot a dichromat segment
#'
#' Produces a dichromat segment plot.
#'
#' @param didata (required) a data frame, possibly a result from the
#'   [colspace()] or [dispace()] function, containing values for the 'x'
#'   coordinates as a column (labeled as such).
#' @inheritParams triplot
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "canis")
#' di.flowers <- colspace(vis.flowers, space = "di")
#' plot(di.flowers)
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @importFrom graphics par points text
#'
#' @keywords internal
#'
#' @inherit dispace references

diplot <- function(didata, labels = TRUE, achro = TRUE, achrocol = "grey",
                   achrosize = 0.8, labels.cex = 1, out.lwd = 1, out.lcol = "black",
                   out.lty = 1, square = TRUE, margins = NULL, ...) {

  if (!missing("margins"))
    message("The 'margins' argument is deprecated, and will be ignored. See ?par() for guidance on 
            setting margins in the standard manner.")

  arg <- list(...)

  # Set defaults
  if (square) {
    arg$asp <- 1
  }
  if (is.null(arg$pch)) {
    arg$pch <- "|"
  }

  # Verticy coordinates
  vert <- data.frame(x = c(-1 / sqrt(2), 1 / sqrt(2)), y = c(0, 0))

  # Blank plot w/ segment
  plot(
    0,
    type = "n", xlim = c(-1 / sqrt(2), 1 / sqrt(2)), ylim = c(-0.5, 0.5),
    bty = "n", axes = FALSE, xlab = " ", ylab = " "
  )
  segments(vert$x[1], vert$y[1], vert$x[2], vert$y[2],
    lwd = out.lwd, lty = out.lty, col = out.lcol
  )

  # Add points
  arg$x <- didata$x
  arg$y <- rep(0, length(didata$x))
  do.call(points, arg)

  # Origin
  if (isTRUE(achro)) {
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }

  # Add text (coloured points better as in tcsplot?)
  if (isTRUE(labels)) {
    text(vert,
      labels = c("S", "L"),
      pos = c(2, 4),
      xpd = TRUE,
      cex = labels.cex
    )
  }
}
