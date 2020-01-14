#' Produces a 3D convex hull in tetrahedral colour space
#'
#' @param grid.alpha transparency of the volume polygon grid lines
#' @param grid if `TRUE`, connects the polygon outlining the volume occupied by points (defaults to `TRUE`)
#' @param fill if `TRUE`, fills the volume occupied by points (WARNING: transparency
#' is not saved properly if exported using `rgl.postscript`)(defaults to `TRUE`).
#'
#' @return [tcsvol()] creates a 3D convex hull within a `tcsplot` object.
#'
#' @rdname tcsplot
#' @export
#'

tcsvol <- function(tcsdata, col = "black", alpha = 0.2,
                   grid.alpha = 1, grid = TRUE, fill = TRUE, lwd = 1) {
  if (attr(tcsdata, "clrsp") != "tcs") stop("object is not in tetrahedral color space")

  # check if rgl is installed and loaded
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop(dQuote("rgl"), " package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  if (!isNamespaceLoaded("rgl")) {
    requireNamespace("rgl")
  }

  coords <- tcsdata[, c("x", "y", "z")]
  vol <- t(convhulln(coords, options = "Tv"))

  if (grid) {
    rgl::triangles3d(coords[vol, ],
                     color = col, alpha = grid.alpha, lwd = lwd,
                     front = "lines", back = "lines"
    )
  }

  if (fill) {
    rgl::rgl.triangles(coords[vol, ],
                       alpha = alpha, color = col
    )
  }

  rgl::material3d(alpha = 1)
}
