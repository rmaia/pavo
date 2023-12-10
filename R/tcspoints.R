#' Plot a tetrahedral colour space
#'
#' Plots points in a tetrahedral colour space
#'
#' @return [tcspoints()] adds points to the plot. Points are currently plotted
#' only as spheres to maintain export capabilities.
#'
#' @rdname tcsplot
#' @export
#'

tcspoints <- function(tcsdata, size = 0.02, col = "black", alpha = 1) {
  if (attr(tcsdata, "clrsp") != "tcs") {
    stop("object is not in tetrahedral colour space", call. = FALSE)
  }

  # check if rgl is installed and loaded
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop(dQuote("rgl"), " package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  rgl::spheres3d(tcsdata[, c("x", "y", "z")],
    radius = size, color = col, lit = FALSE, alpha = alpha
  )
}
