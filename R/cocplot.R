#' Plot the colour opponent coding diagram
#'
#' Produces a plot based on the colour opponent coding diagram of Backhaus
#' (1991).
#'
#' @param cocdata (required) a data frame, possibly a result from the
#'   [colspace()] or [categorical()] function, containing values for 'x' and 'y'
#'   coordinates as columns (labeled as such).
#' @param tick.loc a numeric vector specifying the location of tick marks on x &
#'   y axes.
#' @inheritParams triplot
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE)
#' coc.flowers <- colspace(vis.flowers, space = "coc")
#' plot(coc.flowers)
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @keywords internal
#'
#' @inherit coc references

cocplot <- function(cocdata, labels = TRUE, labels.cex = 0.9,
                    tick.loc = c(-12, -9, -6, -3, 3, 6, 9, 12),
                    achro = FALSE, achrosize = 0.8, achrocol = "grey",
                    margins = c(1, 1, 2, 2), square = TRUE, ...) {
  par(mar = margins)

  if (square) {
    par(pty = "s")
  }

  arg <- list(...)

  # Set defaults
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }
  if (is.null(arg$xlim)) {
    arg$xlim <- range(tick.loc)
  }
  if (is.null(arg$ylim)) {
    arg$ylim <- range(tick.loc)
  }
  if (is.null(arg$xlab)) {
    arg$xlab <- ""
  }
  if (is.null(arg$ylab)) {
    arg$ylab <- ""
  }
  if (is.null(arg$bty)) {
    arg$bty <- "n"
  }

  # Plot
  arg$x <- cocdata$x
  arg$y <- cocdata$y
  arg$axes <- FALSE
  arg$type <- "n"

  do.call(plot, arg)
  axis(1, at = tick.loc, pos = 0, cex.axis = 0.8) # todo - best way to handle user specs?
  axis(2, at = tick.loc, pos = 0, cex.axis = 0.8, las = 2)

  # Origin point
  if (isTRUE(achro)) {
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }

  # remove plot-specific args, add points after the stuff is drawn
  arg[c(
    "type", "xlim", "ylim", "log", "main", "sub", "xlab", "ylab",
    "ann", "axes", "frame.plot", "panel.first", "panel.last", "asp"
  )] <- NULL
  do.call(points, arg)

  # Category labels (todo: make this more flexible/robust?)
  if (labels) {
    text(
      x = c(max(tick.loc), 0),
      y = c(0, max(tick.loc)),
      labels = c("A", "B"),
      pos = c(4, 3),
      xpd = TRUE,
      cex = labels.cex
    )
  }
}
