#' Produces a 3D alphashape in tetrahedral colour space
#'
#' @inheritParams tcsvol
#' @inheritParams tetrashape
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' tcsplot(tcs.sicalis, col = "blue", size = 0.005)
#' tcsshape(tcs.sicalis, avalue = 0.1)
#' }

tcsshape <- function (tcsdata, col = "black", alpha = 0.2, grid.alpha = 1,
                      grid = TRUE, fill = TRUE, lwd = 1, avalue) {
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

  ashape <- alphashape3d::ashape3d(as.matrix(tcsdata[, c("x", "y", "z")]), 
                                   alpha = avalue)
  tri <- ashape$triang
  vol <- t(tri[tri[, ncol(tri)] %in% c(2,3), c(1, 2, 3)])
  coords <- ashape$x

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
