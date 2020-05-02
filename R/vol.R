#' Plot a tetrahedral colour space
#'
#' Produces a 3D colour volume in tetrahedral colour space when plotting a
#' non-interactive tetrahedral plot.
#'
#' @inheritParams tcsplot
#' @inheritParams voloverlap
#' @param avalue if `type = "alpha"`, which alpha parameter value should be used
#'   to compute the alphashape.
#' @param alpha transparency of volume (if `fill = TRUE`).
#' @param grid logical. if `TRUE` (default), draws the polygon outline defined by the points.
#' @param fill logical. if `TRUE` (default), fills the volume defined by the points.
#' @param new logical. Should a new plot be started or draw over an open plot?
#' (defaults to `FALSE`)
#' @param ... additional graphical options. See [polygon()] and [tetraplot()].
#'
#' @return [vol()] creates a 3D colour volume within a static tetrahedral plot.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Hugo Gruson
#'
#' @export
#'
#' @examples
#'
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' plot(tcs.sicalis)
#' vol(tcs.sicalis)
#'
#' @importFrom geometry convhulln
#' @importFrom graphics par polygon
#' @importFrom grDevices trans3d adjustcolor
#' 
#' @inherit overlap3d references
#'

vol <- function(tcsdata, type = c("convex", "alpha"), avalue, alpha = 0.2, 
                grid = TRUE, fill = TRUE, new = FALSE, ...) {
  
  type <- match.arg(type)
  
  if (!is.null(attr(tcsdata, "clrsp")) && attr(tcsdata, "clrsp") != "tcs") {
    stop("object is not in tetrahedral color space")
  }

  if (type == "convex") {
    coords <- tcsdata[, c("x", "y", "z")]
    vol <- t(convhulln(coords, options = "FA")$hull)
  } else {
    ashape <- alphashape3d::ashape3d(as.matrix(tcsdata[, c("x", "y", "z")]), 
                                     alpha = avalue)
    tri <- ashape$triang
    vol <- t(tri[tri[, ncol(tri)] %in% c(2,3), c(1, 2, 3)])
    coords <- ashape$x
  }

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
