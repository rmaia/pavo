#' Plot a static tetrahedral colorspace
#'
#' Produces a static 3D tetrahedral plot.
#'
#' @param tcsdata (required) a data frame, possibly a result from the
#'   [colspace()] or [tcspace()] function, containing values for the 'x', 'y'
#'   and 'z' coordinates as columns (labeled as such).
#' @param theta angle to rotate the plot in the xy plane (defaults to 45).
#' @param phi angle to rotate the plot in the yz plane (defaults to 10).
#' @param perspective logical, should perspective be forced by using point size
#'   to reflect distance from the plane of view? (defaults to `FALSE``)
#' @param range, vert.range multiplier applied to `cex` and `vert.cex`,
#'   respectively, to indicate the size range variation reflecting the distance
#'   from the plane of view.
#' @param r the distance of the eyepoint from the center of the plotting box.
#'   Very high values approximate an orthographic projection (defaults to 1e6).
#'   See [persp()] for details.
#' @param zoom zooms in (values greater than 1) or out (values between 0 and 1)
#'   from the plotting area.
#' @param achro.line logical. Should the achromatic line be plotted? (defaults
#'   to `FALSE`)
#' @param achro.lwd,achro.lty graphical parameters for the achromatic
#'   coordinates.
#' @param tetrahedron logical. Should the tetrahedron be plotted? (defaults to
#'   `TRUE`)
#' @param vert.cex size of the points at the vertices (defaults to 1).
#' @param out.lwd,out.lcol graphical parameters for the tetrahedral outline.
#' @param margin vector of four numbers specifying drawing margins (defaults to
#'   c(0, 0, 0, 0)).
#' @param type accepts a vector of length 1 or 2 with 'p' for points and/or 'l'
#'   for lines from the point to the base of the tetrahedron.

#' @inheritParams triplot
#'
#' @return [tetraplot()] creates a non-interactive 3D plot.
#'
#' @examples
#'
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' plot(tcs.sicalis)
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @export
#'
#' @keywords internal
#'
#' @importFrom graphics grconvertX grconvertY lines par points
#' @importFrom plot3D perspbox
#' @importFrom stats setNames
#' @importFrom utils combn
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage
#'   color in a tetrahedral color space: A phylogenetic analysis of new world
#'   buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour
#'   patterns as birds see them. Biological Journal Of The Linnean Society,
#'   86(4), 405-431.

tetraplot <- function(tcsdata, theta = 45, phi = 10, perspective = FALSE,
                      range = c(1, 2), r = 1e6, zoom = 1,
                      achro = TRUE, achro.col = "grey", achro.size = 1, achro.line = FALSE, achro.lwd = 1, achro.lty = 3,
                      tetrahedron = TRUE, vert.cex = 1, vert.range = c(1, 2), out.lwd = 1, out.lcol = "darkgrey",
                      margin = c(0, 0, 0, 0), type = "p", labels = FALSE, gamut = FALSE, ...) {
  trange <- function(x, newmin, newmax) {
    (((x - min(x)) * (newmax - newmin)) / (max(x) - min(x))) + newmin
  }

  # get arguments
  arg <- list(...)

  if (is.null(arg$col)) arg$col <- 1
  if (is.null(arg$cex)) arg$cex <- 1
  if (is.null(arg$pch)) arg$pch <- 19
  if (is.null(arg$xlab)) arg$xlab <- "x"
  if (is.null(arg$ylab)) arg$ylab <- "y"
  if (is.null(arg$zlab)) arg$zlab <- "z"

  col <- arg$col
  arg$col <- NULL


  # tetrahedron vertices
  verts <- matrix(c(
    0, 0, 0.75,
    (-0.5 * sqrt(1.5)), (-1 / (2 * sqrt(2))), -0.25,
    0, (1 / sqrt(2)), -0.25,
    (0.5 * sqrt(1.5)), (-1 / (2 * sqrt(2))), -0.25
  ),
  byrow = TRUE, nrow = 4,
  dimnames = list(c("u", "s", "m", "l"), c("x", "y", "z"))
  )

  # combinations of vertices to make facets
  sides <- verts[combn(seq_len(4), 2), ]
  rownames(sides) <- paste0(
    rep(
      do.call(paste0, data.frame(t(combn(c("u", "s", "m", "l"), 2)))),
      each = 2
    ),
    c(".1", ".2")
  )

  plims <- any(vapply(list(arg$xlim, arg$ylim, arg$zlim), is.null, logical(1)))

  # if no limits are given, estimate based on tetrahedron or tcsdataa limits
  if (plims) {

    # first check if all xyzlim are null
    if (!all(vapply(list(arg$xlim, arg$ylim, arg$zlim), is.null, logical(1)))) {
      stop('"xlim", "ylim" and "zlim" must either all be NULL or all be vectors of length 2', call. = FALSE)
    }

    if (tetrahedron) {
      arg$xlim <- range(verts[, "x"]) / zoom
      arg$ylim <- range(verts[, "y"]) / zoom
      arg$zlim <- range(verts[, "z"]) / zoom
    } else {
      arg$xlim <- range(tcsdata[, "x"])
      arg$ylim <- range(tcsdata[, "y"])
      arg$zlim <- range(tcsdata[, "z"])
    }
  }

  # draw blank 3d plot
  # Using persp directly creates a white rectangle that cannot be removed. So
  # we have to use perspbox instead.
  M <- do.call(perspbox, c(list(
    x = arg$xlim,
    y = arg$ylim,
    z = diag(2) * arg$zlim,
    r = r, box = FALSE, theta = theta, phi = phi
  ), arg))

  # position of points in projected space
  allcoords <- as.matrix(rbind(
    tcsdata[, c("x", "y", "z")],
    verts[, c("x", "y", "z")],
    achro = c(0, 0, 0),
    achrbot = c(0, 0, -0.25)
  ))

  tcoord <- cbind(allcoords, 1) %*% M
  tcoord[, 1] <- tcoord[, 1] / tcoord[, 4]
  tcoord[, 2] <- tcoord[, 2] / tcoord[, 4]
  colnames(tcoord) <- c("x", "y", "depth", "scale")

  # Empty plot
  argblank <- arg

  # CRAN won't accept triple : arguments and persp.default is not exported,
  # so we need to pass arguments by hand
  perspargs <- c(
    "x", "y", "z", "xlim", "ylim", "zlim", "xlab", "ylab", "zlab",
    "main", "sub", "theta", "phi", "r", "d", "scale", "expand", "col", "border",
    "ltheta", "lphi", "shade", "box", "axes", "nticks", "ticktype", "...", ""
  )

  argblank[perspargs] <- NULL
  argblank$xlim <- tcoord["achro", "x"] + c(-1, 1) * max(abs(tcoord["achro", "x"] - tcoord[, "x"])) / zoom
  argblank$ylim <- tcoord["achro", "y"] + c(-1, 1) * max(abs(tcoord["achro", "y"] - tcoord[, "y"])) / zoom
  # argblank$ylim <- range(tcoord[,'y'])
  argblank$x <- tcoord
  argblank$type <- "n"
  argblank$bty <- "n"
  argblank$xaxt <- "n"
  argblank$yaxt <- "n"
  argblank$ylab <- ""
  argblank$xlab <- ""

  if (!plims) {
    prange <- cbind(x = arg$xlim, y = arg$ylim, z = arg$zlim, 1) %*% M
    prange[, 1] <- prange[, 1] / prange[, 4]
    prange[, 2] <- prange[, 2] / prange[, 4]

    argblank$xlim <- prange[, 1]
    argblank$ylim <- prange[, 2]
  }


  par(mar = margin, pty = "s", new = TRUE)
  do.call(plot, argblank)

  # Get point coordinates
  xy <- tcoord[rownames(tcoord) %in% rownames(tcsdata), c("x", "y"), drop = FALSE]

  # get depth vector
  if (tetrahedron) {
    dvals <- tcoord[, "depth"]
  } else {
    dvals <- tcoord[!rownames(tcoord) %in% c("u", "s", "m", "l"), "depth"]
  }

  # transform depth vector
  dvals <- trange(dvals, range[1], range[2])

  # square root so it scales by area
  dvals <- sqrt(dvals)
  dvals <- trange(dvals, range[1], range[2])

  maxdatad <- max(dvals[names(dvals) %in% rownames(tcsdata)])
  mindatad <- min(dvals[names(dvals) %in% rownames(tcsdata)])

  # turn depth vector to point size
  psize <- dvals * arg$cex

  vrange <- vert.cex * vert.range
  psize[c("u", "s", "m", "l")] <- trange(psize[c("u", "s", "m", "l")], vrange[1], vrange[2])

  # distort if ranges are not the same for points and vertices
  if (!identical(range, vert.range)) {
    psize[names(psize) %in% rownames(tcsdata)] <-
      trange(psize[names(psize) %in% rownames(tcsdata)], range[1], range[2])
  }


  if (!perspective) {
    psize[] <- arg$cex
    psize[c("u", "s", "m", "l")] <- vert.cex
  }


  # add tetrahedron lines and vertices behind the data

  if (tetrahedron) {

    # vertice colours
    vcols <- setNames(
      c("darkorchid1", "cornflowerblue", "mediumseagreen", "firebrick1"),
      rownames(verts)
    )

    # tetrahedron sides
    xytet <- cbind(sides, 1) %*% M
    xytet[, 1] <- xytet[, 1] / xytet[, 4]
    xytet[, 2] <- xytet[, 2] / xytet[, 4]
    colnames(xytet) <- c("x", "y", "depth", "scale")

    segs <- cbind(xytet[c(1, 3, 5, 7, 9, 11), c("x", "y")], xytet[c(2, 4, 6, 8, 10, 12), c("x", "y")])

    # which vertex are behind data
    vinback <- dvals[c("u", "s", "m", "l")] < mindatad

    # sort segments by proximity
    combdist <- setNames(
      colSums(combn(dvals[c("u", "s", "m", "l")], 2)),
      apply(combn(names(dvals[c("u", "s", "m", "l")]), 2), 2, paste0, collapse = "")
    )

    # get 3 most in back
    inback <- names(sort(combdist, )[seq_len(3)])

    linback <- grepl(paste0(inback, collapse = "|"), rownames(segs))

    segments(
      segs[linback, 1, drop = FALSE],
      segs[linback, 2, drop = FALSE],
      segs[linback, 3, drop = FALSE],
      segs[linback, 4, drop = FALSE],
      lwd = out.lwd, col = out.lcol
    )

    # add vertices behind tetrahedron

    points(tcoord[names(vinback)[vinback], c("x", "y"), drop = FALSE],
      pch = 21,
      cex = psize[names(vinback)[vinback]], col = NULL,
      bg = vcols[names(vinback)[vinback]]
    )
  }


  # add achromatic center if it is behind the data
  if (achro && dvals["achro"] < maxdatad) {
    points(tcoord["achro", c("x", "y"), drop = FALSE],
      col = NULL, bg = achro.col,
      pch = 22, cex = psize["achro"] * achro.size
    )
  }

  # add achromatic line if behind the data
  if (achro.line && dvals["achro"] < maxdatad) {
    lines(tcoord[c("achrbot", "u"), c("x", "y"), drop = FALSE], col = achro.col, lty = achro.lty, lwd = achro.lwd)
  }

  ######################
  # add tcsdata points #
  ######################
  argpoints <- arg
  argpoints[perspargs] <- NULL

  argpoints$col <- col
  if (is.null(argpoints$bg)) argpoints$bg <- col
  argpoints$cex <- psize[names(psize) %in% rownames(tcsdata)]
  argpoints$x <- xy

  # ATTRIBUTES THAT RELATE TO POINTS:
  # pch, cex, col, bg ...?
  if (length(argpoints$col) < dim(argpoints$x)[1]) {
    if (dim(argpoints$x)[1] %% length(argpoints$col) > 0) {
      warning('data object length is not a multiple of "col"', call. = FALSE)
    }

    argpoints$col <- rep(argpoints$col, dim(argpoints$x)[1])[seq(dim(argpoints$x)[1])]
  }

  if (length(argpoints$bg) < dim(argpoints$x)[1]) {
    if (dim(argpoints$x)[1] %% length(argpoints$bg) > 0) {
      warning('data object length is not a multiple of "bg"', call. = FALSE)
    }

    argpoints$bg <- rep(argpoints$bg, dim(argpoints$x)[1])[seq(dim(argpoints$x)[1])]
  }

  if (length(argpoints$cex) < dim(argpoints$x)[1]) {
    if (dim(argpoints$x)[1] %% length(argpoints$cex) > 0) {
      warning('data object length is not a multiple of "cex"', call. = FALSE)
    }

    argpoints$cex <- rep(argpoints$cex, dim(argpoints$x)[1])[seq(dim(argpoints$x)[1])]
  }

  if (length(argpoints$pch) < dim(argpoints$x)[1]) {
    if (dim(argpoints$x)[1] %% length(argpoints$pch) > 0) {
      warning('data object length is not a multiple of "pch"', call. = FALSE)
    }

    argpoints$pch <- rep(argpoints$pch, dim(argpoints$x)[1])[seq(dim(argpoints$x)[1])]
  }

  # sort points by distance
  ptorder <- order(dvals[rownames(tcsdata)])

  if (length(argpoints$x) > 2) argpoints$x <- argpoints$x[ptorder, ]
  argpoints$col <- argpoints$col[ptorder]
  argpoints$bg <- argpoints$bg[ptorder]
  argpoints$cex <- argpoints$cex[ptorder]
  argpoints$pch <- argpoints$pch[ptorder]

  if ("l" %in% type) {
    # calculate bottom of the points
    botpoints <- as.matrix(tcsdata[, c("x", "y", "z")])
    botpoints[, "z"] <- -0.25
    botpoints <- cbind(botpoints, 1) %*% M
    botpoints[, 1] <- botpoints[, 1] / botpoints[, 4]
    botpoints[, 2] <- botpoints[, 2] / botpoints[, 4]
    colnames(botpoints) <- c("x", "y", "depth", "scale")

    arghl <- argpoints
    arghl$x <- NULL
    arghl$x0 <- xy[, "x"]
    arghl$y0 <- xy[, "y"]
    arghl$x1 <- botpoints[, "x"]
    arghl$y1 <- botpoints[, "y"]
    do.call(segments, arghl)
  }

  if ("p" %in% type) {
    do.call(points, argpoints)
  }

  # add achromatic center if it is in front of the data
  if (achro && dvals["achro"] > maxdatad) {
    points(tcoord["achro", c("x", "y"), drop = FALSE],
      col = NULL, bg = achro.col,
      pch = 22, cex = psize["achro"] * achro.size
    )
  }

  # add achromatic line if in front of the data
  if (achro.line && dvals["achro"] > maxdatad) {
    lines(tcoord[c("achrbot", "u"), c("x", "y"), drop = FALSE], col = achro.col, lty = achro.lty, lwd = achro.lwd)
  }

  # add tetrahedron lines and vertices in front of the points
  if (tetrahedron) {
    segments(
      segs[!linback, 1, drop = FALSE],
      segs[!linback, 2, drop = FALSE],
      segs[!linback, 3, drop = FALSE],
      segs[!linback, 4, drop = FALSE],
      lwd = out.lwd, col = out.lcol
    )

    points(tcoord[names(vinback)[!vinback], c("x", "y"), drop = FALSE],
      pch = 21,
      cex = psize[names(vinback)[!vinback]], col = NULL,
      bg = vcols[names(vinback)[!vinback]]
    )

    if (labels) {
      text(
        x = tcoord[c("u", "s", "m", "l"), "x"] + sign(tcoord[c("u", "s", "m", "l"), "x"]) * 5e-8,
        y = tcoord[c("u", "s", "m", "l"), "y"] + sign(tcoord[c("u", "s", "m", "l"), "y"]) * 5e-8,
        labels = c("u", "s", "m", "l")
      )
    }
  }

  # Save plot info
  assign("last_plot.tetra", M, envir = .PlotTetraEnv)

  if (gamut) {
    maxgamut <- attr(tcsdata, "data.maxgamut")
    colnames(maxgamut) <- c("x", "y", "z")
    attr(maxgamut, "clrsp") <- "tcs"
    tryCatch(vol(maxgamut, grid = FALSE),
             error = function(e) warning("Max gamut cannot be plotted.",
                                         call. = FALSE))

  }
}
