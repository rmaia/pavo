#' Perceptually-corrected chromaticity diagrams
#'
#' Plot options for [`jnd2xyz`] objects.
#'
#' @param x (required) the output from a [jnd2xyz()] call.
#' @param arrow If and how arrows indicating receptor vectors should be drawn.
#'   Options are `"relative"` (default), `"absolute"` or `"none"`. See
#'   description.
#' @param achro Logical. Should the achromatic variable be plotted as a
#'   dimension? (only available for dichromats and trichromats, defaults to
#'   `FALSE`).
#' @param arrow.labels Logical. Should labels be plotted for receptor arrows?
#'   (defaults to `TRUE`)
#' @param arrow.col color of the arrows and labels.
#' @param arrow.p scaling factor for arrows.
#' @param margin accepts either `"recommended"`, where the function will choose
#'   margin attributes, or a numerical vector of the form `c(bottom, left, top,
#'   right)` which gives the number of lines of margin to be specified on the
#'   four sides of the plot. (Default varies depending on plot dimensionality).
#' @param ... additional parameters to be passed to [plot()], [arrows()] and
#'   [graphics::persp()] (for 3D plots).
#' @inheritParams triplot
#'
#' @return Creates a plot, details of the plot depend on the input data.
#'
#' @note the `arrow` argument accepts three options:
#'   * `"relative"`: With this option, arrows will be made relative to the data.
#'   Arrows will be centered on the data centroid, and will have an arbitrary
#'   length of half the average pairwise distance between points, which can be
#'   scaled with the `arrow.p` argument.
#'   * `"absolute"`: With this option, arrows will be made to reflect the visual
#'   system underlying the data. Arrows will be centered on the achromatic point
#'   in colourspace, and will have length equal to the distance to a
#'   monochromatic point (i.e. a colour that stimulates approximately 99.9% of
#'   that receptor alone). Arrows can still be scaled using the `arrow.p`
#'   argument, in which case they cannot be interpreted as described.
#'   * `"none"`: no arrows will be included.
#'
#' @export
#'
#' @importFrom graphics par points arrows
#' @importFrom grDevices trans3d
#'
#' @keywords internal
#'
#' @examples
#' # Load floral reflectance spectra
#' data(flowers)
#'
#' # Estimate quantum catches visual phenotype of a Blue Tit
#' vis.flowers <- vismodel(flowers, visual = "bluetit")
#'
#' # Estimate noise-weighted colour distances between all flowers
#' cd.flowers <- coldist(vis.flowers)
#'
#' # Convert points to Cartesian coordinates in which Euclidean distances are
#' # noise-weighted.
#' propxyz <- jnd2xyz(cd.flowers)
#'
#' # Plot the floral spectra in 'noise-corrected' space
#' plot(propxyz)
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @references Pike, T.W. (2012). Preserving perceptual distances in
#'   chromaticity diagrams. Behavioral Ecology, 23, 723-728.


jndplot <- function(x, arrow = c("relative", "absolute", "none"), achro = FALSE,
                    arrow.labels = TRUE, arrow.col = "darkgrey", arrow.p = 1, labels.cex = 1,
                    margin = "recommended", square = TRUE, ...) {
  if (achro && !"lum" %in% colnames(x)) {
    stop('"achro" is set to TRUE but data does not have a "lum" column.', call. = FALSE)
  }

  arrow <- match.arg(arrow)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  arg <- list(...)
  if (square) {
    arg$asp <- 1
  }
  if (achro) {
    plotdims <- sum(c("x", "y", "z", "lum") %in% colnames(x))
  } else {
    plotdims <- sum(c("x", "y", "z") %in% colnames(x))
  }

  if (plotdims == 4) {
    stop('cannot use "achro=TRUE" when chromatic space is three-dimensional.', call. = FALSE)
  }

  # CRAN won't accept triple : arguments and persp.default is not exported,
  # so we need to pass arguments by hand
  perspargs <- c(
    "x", "y", "z", "xlim", "ylim", "zlim", "xlab", "ylab", "zlab",
    "main", "sub", "theta", "phi", "r", "d", "scale", "expand", "col", "border",
    "ltheta", "lphi", "shade", "box", "axes", "nticks", "ticktype", "...", ""
  )


  # 1 DIMENSION
  if (plotdims == 1) {
    if (!is.null(margin)) {
      if (margin == "recommended") {
        par(mar = c(5.1, 2.1, 4.1, 2.1))
      } else {
        par(mar = margin)
      }
    }

    # combine data with references
    x2 <- x[, "x", drop = FALSE]

    dat <- rbind(x2, attr(x, "resref")[, "x", drop = FALSE])
    attr(dat, "resref") <- attr(x, "resref")[, "x", drop = FALSE]


    # get arrow coordinates

    arrowstart <- switch(arrow,
      relative = rep(attr(dat, "resref")[4, 1], 2),
      absolute = rep(attr(dat, "resref")[1, 1], 2),
      none = NULL
    )

    arrowpos <- switch(arrow,
      relative = (range(x2[, "x"]) - arrowstart) * arrow.p + arrowstart,
      absolute = (attr(dat, "resref")[2:3, "x"] - arrowstart) * arrow.p + arrowstart,
      none = NULL
    )

    if (is.null(arg$pch)) arg$pch <- "|"

    # Blank plot w/ segment
    plotarg <- arg
    plotarg[names(as.list(args(arrows)))] <- NULL
    plotarg$x <- 0
    plotarg$y <- 0
    plotarg$type <- "n"
    plotarg$ylim <- 0.5 * c(-1, 1)
    if (is.null(plotarg$bty)) plotarg$bty <- "n"
    if (is.null(plotarg$yaxt)) plotarg$yaxt <- "n"
    if (is.null(plotarg$xlab)) plotarg$xlab <- "x"
    if (is.null(plotarg$ylab)) plotarg$ylab <- ""
    if (is.null(plotarg$xlim)) plotarg$xlim <- range(c(x[, "x"], arrowpos))


    do.call(plot, plotarg)


    # add arrows
    if (arrow != "none") {
      arrowarg <- arg
      arrowarg <- arrowarg[names(arg) %in% names(formals(arrows))]
      arrowarg$x0 <- arrowstart
      arrowarg$x1 <- arrowpos
      arrowarg$y0 <- rep(0, 2)
      arrowarg$y1 <- rep(0, 2)
      arrowarg$col <- arrow.col

      do.call(arrows, arrowarg)
    }

    if (arrow.labels) {
      text(c("S", "L"),
        x = arrowarg$x1, y = 0, xpd = TRUE,
        pos = c(2, 4), cex = labels.cex, col = arrow.col
      )
    }

    # Add points
    pointsarg <- arg
    pointsarg[names(formals(arrows))] <- NULL
    pointsarg$col <- arg$col

    pointsarg$x <- x[, "x"]
    pointsarg$y <- rep(0, length(x[, "x"]))

    do.call(points, pointsarg)
  }

  # 2 DIMENSIONS
  if (plotdims == 2) {
    if (!is.null(margin)) {
      if (margin == "recommended") {
        par(mar = c(5.1, 4.1, 4.1, 2.1))
      } else {
        par(mar = margin)
      }
    }

    # select columns to use
    if (achro) {
      colstouse <- c("x", "lum")
    } else {
      colstouse <- c("x", "y")
    }

    # combine data with references
    x2 <- x[, colstouse]
    attr(x, "resref") <- attr(x, "resref")[, colstouse]

    dat <- rbind(x[, colstouse], attr(x, "resref")[, colstouse])
    attr(dat, "resref") <- attr(x, "resref")[, colstouse]


    if (arrow != "none") {
      # get arrow coordinates
      arrowstart <- switch(arrow,
        relative = as.matrix(attr(dat, "resref")[dim(attr(dat, "resref"))[1], colstouse]),
        absolute = as.matrix(attr(dat, "resref")["jnd2xyzrrf.achro", colstouse]),
        none = NULL
      )

      arrowindex <- seq(length(rownames(attr(dat, "resref"))) - 1)[-1]


      # Find angle (atan2(y2-y1, x2-x1))
      endpoints <- as.matrix(attr(dat, "resref")[arrowindex, colstouse])
      angle <- apply(sweep(endpoints, 2, arrowstart, "-"), 1, function(s) atan2(s[2], s[1]))

      rad <- mean(dist(x2)) / 2 * arrow.p

      # find points y=dist*sin, x=dist*cos
      arrowlims <- sweep(cbind(rad * cos(angle), rad * sin(angle)), 2, arrowstart, "+")
      rownames(arrowlims) <- rownames(endpoints)
      colnames(arrowlims) <- colnames(endpoints)

      arrowpos <- switch(arrow,
        relative = sweep(
          sweep(
            arrowlims,
            2, arrowstart, "-"
          ) * arrow.p * 0.9,
          2, arrowstart, "+"
        ),
        absolute = sweep(
          sweep(
            as.matrix(attr(dat, "resref")[arrowindex, colstouse]),
            2, arrowstart, "-"
          ) * arrow.p * 0.9,
          2, arrowstart, "+"
        )
      )

      labelpos <- switch(arrow,
        relative = sweep(
          sweep(
            arrowlims,
            2, arrowstart, "-"
          ) * arrow.p,
          2, arrowstart, "+"
        ),
        absolute = sweep(
          sweep(
            as.matrix(attr(dat, "resref")[arrowindex, colstouse]),
            2, arrowstart, "-"
          ) * arrow.p,
          2, arrowstart, "+"
        )
      )

      if (achro) {
        arrowpos[, "lum"] <- arrowstart[, "lum"]
        arrowpos <- rbind(arrowpos, c(
          arrowstart[, colstouse[1]],
          0.9 * arrow.p * (max(dat[, "lum"]) - arrowstart[, "lum"]) + arrowstart[, "lum"]
        ))
        labelpos[, "lum"] <- arrowstart[, "lum"]
        labelpos <- rbind(labelpos, c(
          arrowstart[, colstouse[1]],
          arrow.p * (max(dat[, "lum"]) - arrowstart[, "lum"]) + arrowstart[, "lum"]
        ))
      }
    }

    if (is.null(arg$pch)) arg$pch <- 19

    # Blank plot w/ segment
    plotarg <- arg
    plotarg[names(formals(arrows))] <- NULL
    plotarg$x <- 0
    plotarg$y <- 0
    plotarg$type <- "n"

    if (is.null(plotarg$xlab)) plotarg$xlab <- "x"
    if (is.null(plotarg$ylab)) {
      if (achro) {
        plotarg$ylab <- "Luminance"
      } else {
        plotarg$ylab <- "y"
      }
    }
    if (arrow != "none") {
      if (is.null(plotarg$xlim)) plotarg$xlim <- range(rbind(x2, labelpos)[, colstouse[1]])
      if (is.null(plotarg$ylim)) plotarg$ylim <- range(rbind(x2, labelpos)[, colstouse[2]])
    } else {
      if (is.null(plotarg$xlim)) plotarg$xlim <- range(rbind(x2)[, colstouse[1]])
      if (is.null(plotarg$ylim)) plotarg$ylim <- range(rbind(x2)[, colstouse[2]])
    }

    do.call(plot, plotarg)


    # add arrows
    if (arrow != "none") {
      arrowarg <- arg
      arrowarg <- arrowarg[names(arg) %in% names(formals(arrows))]
      arrowarg$x0 <- rep(arrowstart[, colstouse[1]], 3)
      arrowarg$x1 <- arrowpos[, colstouse[1]]
      arrowarg$y0 <- rep(arrowstart[, colstouse[2]], 3)
      arrowarg$y1 <- arrowpos[, colstouse[2]]
      arrowarg$col <- arrow.col


      do.call(arrows, arrowarg)
    }

    if (arrow != "none" && arrow.labels) {
      if (achro) {
        lbl <- c("S", "L", "lum")
      } else {
        lbl <- c("S", "M", "L")
      }

      text(labelpos, lbl, xpd = TRUE, cex = labels.cex, col = arrow.col)
    }


    # Add points
    pointsarg <- arg
    pointsarg[names(formals(arrows))] <- NULL
    pointsarg$col <- arg$col

    pointsarg$x <- x[, colstouse[1]]
    pointsarg$y <- x[, colstouse[2]]

    do.call(points, pointsarg)
  }


  # 3 DIMENSIONS
  if (plotdims == 3) {
    if (!is.null(margin)) {
      if (margin == "recommended") {
        par(mar = c(1, 2, 0, 1) + 0.1)
      } else {
        par(mar = margin)
      }
    }

    # select columns to use
    if (achro) {
      colstouse <- c("x", "y", "lum")
    } else {
      colstouse <- c("x", "y", "z")
    }

    # combine data with references
    x2 <- x[, colstouse]
    attr(x2, "resref") <- attr(x, "resref")[, colstouse]

    dat <- rbind(x2, attr(x, "resref")[, colstouse])
    attr(dat, "resref") <- attr(x, "resref")[, colstouse]

    if (arrow != "none") {
      arrowstart <- switch(arrow,
        relative = as.matrix(attr(dat, "resref")[dim(attr(dat, "resref"))[1], colstouse]),
        absolute = as.matrix(attr(dat, "resref")["jnd2xyzrrf.achro", colstouse]),
        none = NULL
      )


      arrowindex <- seq(length(rownames(attr(dat, "resref"))) - 1)[-1]


      # Find angles (atan2(y2-y1, x2-x1))
      endpoints <- as.matrix(attr(dat, "resref")[arrowindex, colstouse])
      anglethe <- apply(sweep(endpoints, 2, arrowstart, "-"), 1, function(s) atan2(s[2], s[1]))
      anglephi <- apply(sweep(endpoints, 2, arrowstart, "-"), 1, function(s) acos(s[3] / sqrt(s[1]^2 + s[2]^2 + s[3]^2)))

      rad <- mean(dist(x2)) * arrow.p / 2

      # find points based on spherical coordinates
      arrowlims <- sweep(cbind(
        rad * sin(anglephi) * cos(anglethe),
        rad * sin(anglephi) * sin(anglethe),
        rad * cos(anglephi)
      ), 2, arrowstart, "+")


      rownames(arrowlims) <- rownames(endpoints)
      colnames(arrowlims) <- colnames(x2)

      arrowpos <- switch(arrow,
        relative = sweep(
          sweep(
            arrowlims,
            2, arrowstart, "-"
          ) * arrow.p * 0.9,
          2, arrowstart, "+"
        ),
        absolute = sweep(
          sweep(
            as.matrix(attr(dat, "resref")[arrowindex, colstouse]),
            2, arrowstart, "-"
          ) * arrow.p * 0.9,
          2, arrowstart, "+"
        )
      )

      labelpos <- switch(arrow,
        relative = sweep(
          sweep(
            arrowlims,
            2, arrowstart, "-"
          ) * arrow.p,
          2, arrowstart, "+"
        ),
        absolute = sweep(
          sweep(
            as.matrix(attr(dat, "resref")[arrowindex, colstouse]),
            2, arrowstart, "-"
          ) * arrow.p,
          2, arrowstart, "+"
        )
      )

      if (achro) {
        arrowpos[, "lum"] <- arrowstart[, "lum"]
        arrowpos <- rbind(arrowpos, c(
          arrowstart[, colstouse[1:2]],
          0.9 * arrow.p * (max(dat[, "lum"]) - arrowstart[, "lum"]) + arrowstart[, "lum"]
        ))
        labelpos[, "lum"] <- arrowstart[, "lum"]
        labelpos <- rbind(labelpos, c(
          arrowstart[, colstouse[1:2]],
          arrow.p * (max(dat[, "lum"]) - arrowstart[, "lum"]) + arrowstart[, "lum"]
        ))
      }
    }

    if (is.null(arg$pch)) arg$pch <- 19

    # Blank plot w/ segment
    plotarg <- arg
    plotarg[names(formals(arrows))] <- NULL
    if (arrow != "none") {
      if (is.null(plotarg$xlim)) plotarg$xlim <- range(rbind(x2, labelpos)[, colstouse[1]])
      if (is.null(plotarg$ylim)) plotarg$ylim <- range(rbind(x2, labelpos)[, colstouse[2]])
      if (is.null(plotarg$zlim)) plotarg$zlim <- range(rbind(x2, labelpos)[, colstouse[3]])
    } else {
      if (is.null(plotarg$xlim)) plotarg$xlim <- range(rbind(x2)[, colstouse[1]])
      if (is.null(plotarg$ylim)) plotarg$ylim <- range(rbind(x2)[, colstouse[2]])
      if (is.null(plotarg$zlim)) plotarg$zlim <- range(rbind(x2)[, colstouse[3]])
    }
    plotarg$x <- plotarg$xlim
    plotarg$y <- plotarg$ylim
    plotarg$z <- matrix(c(plotarg$zlim, plotarg$zlim), nrow = 2)

    if (is.null(plotarg$xlab)) plotarg$xlab <- "x"
    if (is.null(plotarg$ylab)) plotarg$ylab <- "y"
    if (is.null(plotarg$zlab)) {
      if (achro) {
        plotarg$zlab <- "Luminance"
      } else {
        plotarg$zlab <- "z"
      }
    }

    plotarg$col <- NA
    plotarg$border <- NA
    if (is.null(plotarg$box)) plotarg$box <- TRUE
    if (is.null(plotarg$theta)) plotarg$theta <- 40
    if (is.null(plotarg$phi)) plotarg$phi <- 10
    if (is.null(plotarg$d)) plotarg$d <- 1e6
    if (is.null(plotarg$ticktype)) plotarg$ticktype <- "detailed"

    P <- do.call(perspbox, plotarg)


    # Add arrows and labels
    if (arrow != "none") {
      astart <- trans3d(
        rep(arrowstart[, colstouse[1]], 4),
        rep(arrowstart[, colstouse[2]], 4),
        rep(arrowstart[, colstouse[3]], 4), P
      )

      aend <- trans3d(
        arrowpos[, colstouse[1]],
        arrowpos[, colstouse[2]],
        arrowpos[, colstouse[3]], P
      )

      arrowarg <- arg
      arrowarg <- arrowarg[names(arg) %in% names(formals(arrows))]

      arrowarg$x0 <- astart$x
      arrowarg$x1 <- aend$x
      arrowarg$y0 <- astart$y
      arrowarg$y1 <- aend$y
      arrowarg$col <- arrow.col

      do.call(arrows, arrowarg)

      if (arrow.labels) {
        if (achro) {
          lbl <- c("S", "M", "L", "lum")
        } else {
          lbl <- c("U", "S", "M", "L")
        }

        lpos <- trans3d(labelpos[, 1], labelpos[, 2], labelpos[, 3], P)

        text(lpos, lbl, xpd = TRUE, cex = labels.cex, col = arrow.col)
      }
    }

    # Add points
    pointsarg <- arg
    pointsarg[names(formals(arrows))] <- NULL
    pointsarg[setdiff(
      perspargs,
      names(formals(plot.default))
    )] <- NULL
    pointsarg$col <- arg$col

    ptpos <- trans3d(x2[, colstouse[1]], x2[, colstouse[2]], x2[, colstouse[3]], P)

    pointsarg$x <- ptpos$x
    pointsarg$y <- ptpos$y

    do.call(points, pointsarg)

    # Save plot info

    assign("last_plot.jnd2xyz", P, envir = .PlotJND2XYZEnv)
  }
}
