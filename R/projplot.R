#' 2D projection of a tetrahedral colourspace
#'
#' Produces a 2D projection plot of points in a tetrahedral colour space
#'
#' @param tcsdata (required) tetrahedral color space coordinates, possibly a result from [colspace()],
#' containing values for the 'h.theta' and 'h.phi' coordinates as columns (labeled as such).
#' @param ... additional parameters to be passed to the plotting of data points.
#'
#' @return [projplot()] creates a 2D plot  of color points projected from the tetrahedron
#' to its encapsulating sphere, and is ideal to visualize differences in hue.
#' @note [projplot()] uses the Mollweide projection, and not the Robinson projection, which
#' has been used in the past. Among other advantages, the Mollweide projection preserves area
#' relationships within latitudes without distortion.
#'
#' @export
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @examples
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' projplot(tcs.sicalis, pch = 16, col = setNames(rep(seq_len(3), 7), rep(c("C", "T", "B"), 7)))
#' @inherit tcspace references

projplot <- function(tcsdata, ...) {

  # Check for mapproj
  if (!requireNamespace("mapproj", quietly = TRUE)) {
    stop("Package \"mapproj\" needed for projection plots. Please install it.",
         call. = FALSE
    )
  }

  # oPar <- par(no.readonly=TRUE)
  oPar <- par("mar")
  on.exit(par(oPar))

  # plot
  cu <- "#984EA3"
  cs <- "#377EB8"
  cm <- "#4DAF4A"
  cl <- "#E41A1C"

  par(mar = c(0, 0, 0, 0))
  plot(0, 0,
    axes = FALSE, xlab = "", ylab = "", type = "n", frame.plot = FALSE,
    xlim = c(-2, 2), ylim = c(-1, 1)
  )

  # We need to call mapproject() before calling map.grid() in order to assign
  # a value to mapproj::.Last.project
  mapproj::mapproject(0, 0, projection = "mollweide")
  mapproj::map.grid(c(-180, 180, -90, 90), labels = FALSE, col = "grey")

  x <- c(-1.224647e-16, -1.224647e-16, 1.224647e-16, 1.224647e-16, -1.928477, 1.928477, -6.428450e-01, 6.428450e-01, -4.465816e-08)
  y <- c(-1, 1, -1, 1, -0.2649321, -0.2649321, -0.2649321, -0.2649321, 1.0000000)

  points(y ~ x,
    pch = 20, cex = 1.5,
    col = c(rep("grey", 4), cl, cl, cm, cs, cu)
  )

  projpoints(tcsdata, ...)
}
