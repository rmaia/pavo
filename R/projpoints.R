#' Plot a tetrahedral color space
#'
#' Adds points to a tetrahedral colorspace projection
#'
#' @return \code{projpoints} creates points in a projection color space plot
#' produced by \code{projplot}.
#'
#' @rdname projplot
#'
#' @export

projpoints <- function(tcsdata, ...) {

  # Check for mapproj
  if (!requireNamespace("mapproj", quietly = TRUE)) {
    stop("Package \"mapproj\" needed for projection plots. Please install it.",
         call. = FALSE)
  }

  points.theta <- tcsdata[, "h.theta"]
  points.phi <- tcsdata[, "h.phi"]

  n <- length(points.theta)

  # Edges of the tetrahedron, adjusted
  vert.theta <- c(-3.1415, 3.1415, -1.047198, 1.047198, -2.617994)
  vert.phi <- c(-0.3398369, -0.3398369, -0.3398369, -0.3398369, 1.5707963)

  # Edges of the figure
  edge.theta <- c(-pi, -pi, pi, pi)
  edge.phi <- c(-pi / 2, pi / 2, -pi / 2, pi / 2)

  # adjust points

  points.theta <- ifelse(points.theta >= -0.5235988,
    points.theta - (150 / 180 * pi),
    points.theta + (210 / 180 * pi)
  )


  # radians to degrees
  coords.theta <- c(edge.theta, vert.theta, points.theta) * 180 / pi
  coords.phi <- c(edge.phi, vert.phi, points.phi) * 180 / pi

  # map projection coordinates

  mp <- mapproj::mapproject(coords.theta, coords.phi, projection = "mollweide")

  mp.v.theta <- mp$x[1:9]
  mp.v.phi <- mp$y[1:9]

  mp.p.theta <- mp$x[-c(1:9)]
  mp.p.phi <- mp$y[-c(1:9)]

  points(mp.p.phi ~ mp.p.theta, ...)

  # par(par.old)
}
