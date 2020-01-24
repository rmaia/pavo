#' Plot a (possibly concave) tetrahedral colour space
#'
#' Produces a 3D alphashape in tetrahedral colour space when plotting a
#' non-interactive tetrahedral plot.
#'
#' @inheritParams vol
#' @param avalue Value of alpha parameter for the alphashape
#'
#' @examples
#'
#' @examples
#' \dontrun{
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' plot(tcs.sicalis)
#' tetrashape(tcs.sicalis, avalue = 0.1)
#' }
#'
#' @export
#'
#' @importFrom  alphashape3d ashape3d

tetrashape <- function(tcsdata, alpha = 0.2, grid = TRUE, fill = TRUE,
                       new = FALSE, avalue, ...) {
  if (!is.null(attr(tcsdata, "clrsp")) && attr(tcsdata, "clrsp") != "tcs")
    stop("object is not in tetrahedral color space")

  ashape <- ashape3d(as.matrix(tcsdata[, c("x", "y", "z")]), alpha = avalue)

  tri <- ashape$triang
  vol <- t(tri[tri[, ncol(tri)] %in% c(2,3), c(1, 2, 3)])
  coords <- ashape$x

  arg <- list(...)

  if (new) {
    argempty <- c(list(border = FALSE), arg)

    argempty$col <- NULL

    if (is.null(argempty$zoom)) {
      argempty$zoom <- 1
    }
    if (is.null(argempty$xlim)) {
      argempty$xlim <- range(tcsdata[, "x"]) / argempty$zoom
    }
    if (is.null(argempty$ylim)) {
      argempty$ylim <- range(tcsdata[, "y"]) / argempty$zoom
    }
    if (is.null(arg$zlim)) {
      argempty$zlim <- range(tcsdata[, "z"]) / argempty$zoom
    }

    argempty$zoom <- NULL

    if (is.null(argempty$theta)) {
      argempty$theta <- 45
    }
    if (is.null(argempty$phi)) {
      argempty$phi <- 10
    }
    if (is.null(argempty$r)) {
      argempty$r <- 12
    }
    if (is.null(argempty$box)) {
      argempty$box <- FALSE
    }

    if (is.null(argempty$margin)) {
      margin <- c(0, 0, 0, 0)
    } else {
      margin <- argempty$margin
      argempty$margin <- NULL
    }

    argempty$x <- argempty$xlim
    argempty$y <- argempty$ylim
    argempty$z <- matrix(c(argempty$zlim, argempty$zlim), nrow = 2)

    # empty plot
    oPar <- par("mar", "pty")
    on.exit(par(oPar))

    par(mar = margin)

    P <- do.call(perspbox, argempty)

    # Save plot info
    assign("last_plot.tetra", P, envir = .PlotTetraEnv)
  }


  last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)

  flatcoords <- data.frame(trans3d(coords[, "x"], coords[, "y"], coords[, "z"], last_tetraplot))

  if (is.null(arg$col)) {
    arg$col <- "darkgrey"
  }

  darkcolor <- arg$col
  alphacolor <- adjustcolor(arg$col, alpha.f = alpha)

  if (fill) {
    arg$border <- NA
    arg$col <- alphacolor
  } else {
    arg$col <- NA
  }

  if (grid) {
    arg$border <- darkcolor
  }

  # CRAN won't accept triple : arguments and persp.default is not exported,
  # so we need to pass arguments by hand
  perspargs <- c(
    "x", "y", "z", "xlim", "ylim", "zlim", "xlab", "ylab", "zlab",
    "main", "sub", "theta", "phi", "r", "d", "scale", "expand",
    "ltheta", "lphi", "shade", "box", "axes", "nticks", "ticktype", "...", ""
  )

  arg[perspargs] <- NULL


  for (i in seq_len(ncol(vol))) {
    arg$x <- flatcoords[vol[, i], "x"]
    arg$y <- flatcoords[vol[, i], "y"]

    do.call(polygon, arg)
  }
}
