#' Plot points in a colourspace
#'
#' Add points to a colourspace plot
#'
#'
#' @inheritParams plot.colspace
#' @param ... additional graphical options. See [par()].
#'
#' @return `points.colspace` adds points to a colourspace plot.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @importFrom graphics points
#' @importFrom grDevices trans3d
#'
#' @seealso [plot.colspace()]
#'
#' @export
#'

points.colspace <- function(x, ...) {
  # Defaults in line with those in the plots
  arg <- list(...)
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }

  if (attr(x, "clrsp") == "tcs") {
    last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)
    xy <- trans3d(x[, "x"], x[, "y"], x[, "z"], last_tetraplot)
    do.call(points, c(xy, arg))
  } else if (attr(x, "clrsp") == "CIELAB") {
    last_labplot <- get("last_plot.cielab", envir = .PlotCielabEnv)
    xy <- trans3d(x[, "a"], x[, "b"], x[, "L"], last_labplot)
    do.call(points, c(xy, arg))
  } else {
    arg$x <- x[, "x"]
    arg$y <- x[, "y"]

    do.call(points, arg)
  }
}
