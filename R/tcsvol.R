#' Plot a tetrahedral color space
#'
#' Produces a 3D convex hull in tetrahedral color space
#'
#' @param grid.alpha transparency of the volume polygon grid lines
#' @param grid if \code{TRUE}, connects the polygon outlining the volume occupied by points (defaults to \code{TRUE})
#' @param fill if \code{TRUE}, fills the volume occupied by points (WARNING: transparency
#' is not saved properly if exported using \code{rgl.postscript})(defaults to \code{TRUE}).
#'
#' @return \code{tcsvol} creates a 3D convex hull within a \code{tcsplot} object.
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

  vol <- t(convhulln(tcsdata[, c("x", "y", "z")], options = "FA")$hull)
  coords <- tcsdata[, c("x", "y", "z")]
  listvol <- split(vol, rep(seq_len(ncol(vol)), each = nrow(vol)))
  ppairs <- do.call(rbind, lapply(listvol, function(x) t(combn(x, 2))))

  if (grid) {
    for (i in seq_len(nrow(ppairs))) {
      rgl::segments3d(coords[ppairs[i, ], "x"],
        coords[ppairs[i, ], "y"],
        coords[ppairs[i, ], "z"],
        color = col, alpha = grid.alpha, lwd = lwd
      )
    }
  }

  if (fill) {
    rgl::rgl.triangles(coords[vol, 1], coords[vol, 2], coords[vol, 3],
      alpha = alpha, color = col
    )
  }

  rgl::material3d(alpha = 1)
}
