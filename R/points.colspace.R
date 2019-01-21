#' Plot points in a colourspace
#'
#' Add points to a colourspace plot
#'
#'
#' @inheritParams plot.colspace
#' @param ... additional graphical options. See \code{\link{par}}.
#'
#' @return \code{points.colspace} adds points to a colorspace plot. When \code{space = 'tcs'},
#'  it creates 3D points in a tetrahedral color space plot using functions of the package \code{rgl},
#'  based on openGL capabilities.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'

points.colspace <- function(x, ...) {

  # Defaults in line with those in the plots
  arg <- list(...)

  # if(is.null(arg$col))
  #  arg$col <- 'forestgreen'
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }

  if (attr(x, "clrsp") != "tcs" & attr(x, "clrsp") != "CIELAB") {
    arg$x <- x[, "x"]
    arg$y <- x[, "y"]

    do.call(points, arg)
  }

  if (attr(x, "clrsp") == "tcs") {
    last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)

    # arg$x <- x[ ,'x']
    # arg$y <- x[ ,'y']
    # arg$z <- x[ ,'z']

    # do.call(last_tetraplot$points3d, arg)

    xy <- trans3d(x[, "x"], x[, "y"], x[, "z"], last_tetraplot)
    do.call(points, c(xy, arg))
  }

  if (attr(x, "clrsp") == "CIELAB") {
    last_labplot <- get("last_plot.cielab", envir = .PlotCielabEnv)

    # arg$x <- x[ ,'a']
    # arg$y <- x[ ,'b']
    # arg$z <- x[ ,'L']

    # do.call(last_labplot$points3d, arg)

    xy <- trans3d(x[, "a"], x[, "b"], x[, "L"], last_labplot)
    do.call(points, c(xy, arg))
  }
}
