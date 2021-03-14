#' CIE plot
#'
#' Plot a CIE (XYZ, LAB, or LCH) chromaticity diagram.
#'
#' @param ciedata (required). a data frame, possibly a result from the
#'   [colspace()] or [cie()] function, containing values for 'x', 'y' and 'z'
#'   coordinates for the CIEXYZ model, or LAB coordinates for the CIELAB (or
#'   CIELCh models), as columns (labeled as such).
#' @param mono should the monochromatic loci (the 'horseshoe') be plotted when
#'   `space = "ciexyz"`? Defaults to `TRUE`.
#' @param theta angle to rotate the plot in the xy plane  when
#'   `space = "cielab"` (defaults to 10).
#' @param phi angle to rotate the plot in the yz plane  when `space = "cielab"`
#'   (defaults to 45).
#' @param r the distance of the eyepoint from the center of the plotting box
#'   when `space = "cielab"`. Very high values approximate an orthographic
#'   projection (defaults to 1e6). See [graphics::persp()] for details.
#' @param zoom zooms in (values greater than 1) or out (values between 0 and 1)
#'   from the plotting area when `space = "cielab"`.
#' @param box logical. Should the plot area box and axes be plotted? (defaults
#'   to `FALSE`)
#' @param ciebg should the colour background be plotted for CIEXYZ plot?
#'   (defaults to `TRUE`)
#' @param margin Deprecated. Please use the standard [par()] method for custom margins.
#' @inheritParams triplot
#'
#' @examples
#' # Load floral reflectance spectra
#' data(flowers)
#'
#' # CIEXYZ
#' # Estimate quantum catches, using the cie10-degree viewer matching function
#' vis.flowers <- vismodel(flowers, visual = "cie10", illum = "D65", vonkries = TRUE, relative = FALSE)
#' 
#' # Run the ciexyz model
#' xyz.flowers <- colspace(vis.flowers, space = "ciexyz")
#' 
#' # Visualise the floral spectra in a ciexyz chromaticity diagram
#' plot(xyz.flowers)
#'
#' # CIELAB
#' # Using the quantum catches above, instead model the spectra in the CIELab
#' # space
#' lab.flowers <- colspace(vis.flowers, space = "cielab")
#' 
#' # And plot in Lab space
#' plot(lab.flowers)
#' 
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export
#'
#' @keywords internal
#'
#' @importFrom graphics par points polygon
#' @importFrom grDevices trans3d
#' @importFrom plot3D perspbox
#' @importFrom magick image_read
#'
#' @inherit cie references


cieplot <- function(ciedata, mono = TRUE, out.lwd = NULL, out.lcol = "black",
                    out.lty = 1, theta = 45, phi = 10, r = 1e6, zoom = 1, box = FALSE,
                    ciebg = TRUE, margin = NULL, ...) {
  if (!missing("margin")) {
    message(
      "The 'margin' argument is deprecated, and will be ignored. ",
      "See ?par() for guidance on setting margins in the standard manner."
    )
  }

  arg <- list(...)

  # CIEXYZ
  if (attr(ciedata, "clrsp") == "CIEXYZ") {

    # Set defaults
    if (is.null(arg$pch)) {
      arg$pch <- 19
    }
    if (is.null(arg$xaxp)) {
      arg$xaxp <- c(0, 0.9, 9)
    }
    if (is.null(arg$asp)) {
      arg$asp <- 1
    }
    if (is.null(arg$yaxp)) {
      arg$yaxp <- c(0, 0.8, 8)
    }
    if (is.null(arg$xlim)) {
      arg$xlim <- c(0, 0.75)
    }
    if (is.null(arg$ylim)) {
      arg$ylim <- c(0, 0.85)
    }
    if (is.null(arg$xlab)) {
      arg$xlab <- "CIE x"
    }
    if (is.null(arg$ylab)) {
      arg$ylab <- "CIE y"
    }

    # Monochromatic loci in XYZ, from Westland et al. 2012
    monox <- c(
      0.175596, 0.172787, 0.170806, 0.170085, 0.160343, 0.146958, 0.139149,
      0.133536, 0.126688, 0.115830, 0.109616, 0.099146, 0.091310, 0.078130,
      0.068717, 0.054675, 0.040763, 0.027497, 0.016270, 0.008169, 0.004876,
      0.003983, 0.003859, 0.004646, 0.007988, 0.013870, 0.022244, 0.027273,
      0.032820, 0.038851, 0.045327, 0.052175, 0.059323, 0.066713, 0.074299,
      0.089937, 0.114155, 0.138695, 0.154714, 0.192865, 0.229607, 0.265760,
      0.301588, 0.337346, 0.373083, 0.408717, 0.444043, 0.478755, 0.512467,
      0.544767, 0.575132, 0.602914, 0.627018, 0.648215, 0.665746, 0.680061,
      0.691487, 0.700589, 0.707901, 0.714015, 0.719017, 0.723016, 0.734674
    )
    monoy <- c(
      0.005295, 0.004800, 0.005472, 0.005976, 0.014496, 0.026643, 0.035211,
      0.042704, 0.053441, 0.073601, 0.086866, 0.112037, 0.132737, 0.170464,
      0.200773, 0.254155, 0.317049, 0.387997, 0.463035, 0.538504, 0.587196,
      0.610526, 0.654897, 0.675970, 0.715407, 0.750246, 0.779682, 0.792153,
      0.802971, 0.812059, 0.819430, 0.825200, 0.829460, 0.832306, 0.833833,
      0.833316, 0.826231, 0.814796, 0.805884, 0.781648, 0.754347, 0.724342,
      0.692326, 0.658867, 0.624470, 0.589626, 0.554734, 0.520222, 0.486611,
      0.454454, 0.424252, 0.396516, 0.372510, 0.351413, 0.334028, 0.319765,
      0.308359, 0.299317, 0.292044, 0.285945, 0.280951, 0.276964, 0.265326
    )

    # Plot
    arg$x <- ciedata$x
    arg$y <- ciedata$y

    do.call(plot, c(arg, type = "n"))

    if (ciebg) {
      bg <- image_read(system.file("ciebg.png", package = "pavo"))
      rasterImage(bg, min(monox), min(monoy), max(monox), max(monoy))
    }

    if (mono == TRUE) {
      polygon(monoy ~ monox, border = out.lcol, lty = out.lty, lwd = out.lwd)
    }

    # remove plot-specific args, add points after the stuff is drawn
    arg[c(
      "type", "xlim", "ylim", "log", "main", "sub", "xlab", "ylab",
      "ann", "axes", "frame.plot", "panel.first", "panel.last", "asp"
    )] <- NULL
    do.call(points, arg)

    invisible(NULL)
  }

  # CIELAB or CIELch
  if (attr(ciedata, "clrsp") == "CIELAB" | attr(ciedata, "clrsp") == "CIELCh") {

    # Set defaults
    arg <- list(...)

    if (is.null(arg$pch)) {
      arg$pch <- 19
    }
    if (is.null(arg$col)) {
      arg$col <- 1
    }
    if (is.null(arg$xlim)) {
      arg$xlim <- c(-128, 127) / zoom
    }
    if (is.null(arg$ylim)) {
      arg$ylim <- c(-128, 127) / zoom
    }
    if (is.null(arg$zlim)) {
      arg$zlim <- c(0, 100) / zoom
    }

    col <- arg["col"]
    arg["col"] <- NULL

    # draw blank 3d plot
    # Using persp directly creates a white rectangle that cannot be removed. So
    # we have to use perspbox instead.

    P <- do.call(perspbox, c(list(
      x = arg$xlim,
      y = arg$ylim,
      z = diag(2) * arg$zlim,
      border = FALSE, r = r, box = box, theta = theta, phi = phi
    ), arg))


    # LAB plot axis line vertices
    verts <- matrix(
      c(0, 0, 0, 0, 0, 100, -128, 0, 50, 127, 0, 50, 0, -128, 50, 0, 127, 50),
      ncol = 3, byrow = TRUE,
      dimnames = list(paste0(rep(c("L", "a", "b"), each = 2), 1:2), c("x", "y", "z"))
    )

    vertst <- trans3d(verts[, "x"], verts[, "y"], verts[, "z"], P)

    # Text label locations

    txtlab <- matrix(
      c(0, 0, 104, -140, 0, 50, 0, -150, 50),
      ncol = 3, byrow = TRUE,
      dimnames = list(c("tL", "ta", "tb"), c("x", "y", "z"))
    )

    txtlabt <- trans3d(txtlab[, "x"], txtlab[, "y"], txtlab[, "z"], P)

    # Draw them up
    segments(vertst$x["L1"], vertst$y["L1"], vertst$x["L2"], vertst$y["L2"], lwd = 1.5)
    segments(vertst$x["a1"], vertst$y["a1"], vertst$x["a2"], vertst$y["a2"], lwd = 1.5)
    segments(vertst$x["b1"], vertst$y["b1"], vertst$x["b2"], vertst$y["b2"], lwd = 1.5)

    # Axis labels
    text(x = txtlabt$x["tL"], txtlabt$y["tL"], labels = "L")
    text(x = txtlabt$x["ta"], txtlabt$y["ta"], labels = "a")
    text(x = txtlabt$x["tb"], txtlabt$y["tb"], labels = "b")

    # Data
    # CRAN won't accept triple : arguments and persp.default is not exported,
    # so we need to pass arguments by hand
    perspargs <- c(
      "x", "y", "z", "xlim", "ylim", "zlim", "xlab", "ylab", "zlab",
      "main", "sub", "theta", "phi", "r", "d", "scale", "expand", "col", "border",
      "ltheta", "lphi", "shade", "box", "axes", "nticks", "ticktype", "...", ""
    )

    argpoints <- arg
    argpoints[perspargs] <- NULL
    argpoints["col"] <- col

    xy <- trans3d(ciedata[, "a"], ciedata[, "b"], ciedata[, "L"], P)

    argpoints$x <- xy$x
    argpoints$y <- xy$y

    do.call(points, argpoints)

    # Save plot info
    # .PlotCielabEnv <<- new.env()
    assign("last_plot.cielab", P, envir = .PlotCielabEnv)
  }
}
