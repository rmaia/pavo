#' Plot a tetrahedral color space
#'
#' Produces a 3D convex hull in tetrahedral color space when plotting a
#' non-interactive tetrahedral plot.
#'
#' @inheritParams tcsplot
#' @param alpha transparency of volume (if \code{fill = TRUE}).
#' @param grid logical. if \code{TRUE} (default), draws the polygon outline defined by the points.
#' @param fill logical. if \code{TRUE} (default), fills the volume defined by the points.
#' @param new logical. Should a new plot be started or draw over an open plot?
#' (defaults to FALSE)
#' @param ... additional graphical options. See \code{\link{polygon}} and \code{\link{tetraplot}}.
#'
#' @return \code{vol} creates a 3D convex hull within a static tetrahedral plot.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export
#'
#' @importFrom geometry convhulln
#'
#' @importFrom grDevices trans3d
#'

vol <- function(tcsdata, alpha = 0.2, grid = TRUE, fill = TRUE,
                new = FALSE, ...) {
  if (!is.null(attr(tcsdata, "clrsp")) && attr(tcsdata, "clrsp") != "tcs") stop("object is not in tetrahedral color space")

  vol <- t(convhulln(tcsdata[, c("x", "y", "z")], options = "FA")$hull)
  coords <- tcsdata[, c("x", "y", "z")]
  listvol <- split(vol, rep(seq_len(ncol(vol)), each = nrow(vol)))
  ppairs <- do.call(rbind, lapply(listvol, function(x) t(combn(x, 2))))

  # check if there is a plot
  isthereplot <- try(get("last_plot.tetra", envir = .PlotTetraEnv), silent = TRUE)

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
  alphacolor <- rgb(t(col2rgb(arg$col)), alpha = alpha * 255, maxColorValue = 255)

  if (fill) {
    arg$border <- NA
    arg$col <- alphacolor
  }

  if (grid) {
    arg$border <- darkcolor
  }

  if (!fill) {
    arg$col <- NA
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
