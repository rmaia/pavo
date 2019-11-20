#' Interactive plot of a tetrahedral colourspace
#'
#' Produces an interactive 3D plot of a tetrahedral colourspace using OpenGL
#'  capabilities.
#'
#' @inheritParams tetraplot
#' @inheritParams triplot
#' @param size size of the points in the plot (defaults to 0.02)
#' @param col colour of the points in the plot (defaults to black)
#' @param alpha transparency of points (or volume fill in [tcsvol()])
#' @param vertexsize size of the points at the vertices
#' @param lwd,lcol graphical parameters for the edges of the tetrahedron.
#' @param new should a new 3D plot be called (defaults to `FALSE`)?
#' @param hspin if `TRUE`, the graphic will spin horizontally (around the 'z' axis)(defaults to `FALSE`)
#' @param vspin if `TRUE`, the graphic will spin vertically (around the 'x' axis)(defaults to `FALSE`)
#' @param floor if `TRUE`, a reference xy plane is plotted under the tetrahedron (defaults to `TRUE`)
#'
#' @return [tcsplot()] creates a 3D plot using functions of the package `rgl`,
#' based on openGL capabilities. Plot is interactive and can be manipulated with the mouse
#' (left button: rotate along 'z' axis; right button: rotate along 'x' axis;
#' third button: zoom).
#'
#' @examples
#' \dontrun{
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' tcsplot(tcs.sicalis, size = 0.005)
#' rgl::rgl.postscript("testplot.pdf", fmt = "pdf")
#' rgl::rgl.snapshot("testplot.png")
#'
#' # For adding points
#' patch <- rep(c("C", "T", "B"), 7)
#' tcs.crown <- subset(tcs.sicalis, "C")
#' tcs.breast <- subset(tcs.sicalis, "B")
#' tcsplot(tcs.crown, col = "blue")
#' tcspoints(tcs.breast, col = "red")
#'
#' # For plotting convex hull
#' tcsplot(tcs.sicalis, col = "blue", size = 0.005)
#' tcsvol(tcs.sicalis)
#' }
#'
#' @seealso [rgl::spheres3d()],[rgl::rgl.postscript()],
#' [rgl::rgl.snapshot()],[rgl::rgl.material()]
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export
#'
#' @inherit tcspace references


# ToDo: Add option to not plot tetrahedron

tcsplot <- function(tcsdata, size = 0.02, alpha = 1, col = "black",
                    vertexsize = 0.02, achro = TRUE, achrosize = 0.01, achrocol = "grey",
                    lwd = 1, lcol = "lightgrey", new = FALSE, hspin = FALSE,
                    vspin = FALSE, floor = TRUE, gamut = FALSE) {

  # check if rgl is installed and loaded
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop(dQuote("rgl"), " package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  if (new) {
    rgl::open3d(FOV = 1, mouseMode = c("zAxis", "xAxis", "zoom"))
  }

  ttv <- ttvertex

  cu <- "#984EA3"
  cs <- "#377EB8"
  cm <- "#4DAF4A"
  cl <- "#E41A1C"

  rgl::plot3d(unlist(ttv[c("xu", "xs", "xm", "xl")]),
    unlist(ttv[c("yu", "ys", "ym", "yl")]),
    unlist(ttv[c("zu", "zs", "zm", "zl")]),
    type = "s", lit = FALSE,
    radius = vertexsize, box = FALSE, axes = FALSE,
    xlab = "", ylab = "", zlab = "",
    col = c(cu, cs, cm, cl)
  )

  rgl::lines3d(unlist(ttv[c("xu", "xs", "xm", "xl", "xs", "xl", "xu", "xm")]),
               unlist(ttv[c("yu", "ys", "ym", "yl", "ys", "yl", "yu", "ym")]),
               unlist(ttv[c("zu", "zs", "zm", "zl", "zs", "zl", "zu", "zm")]),
               color = lcol, lwd = lwd)

  if (achro) {
    rgl::spheres3d(0, 0, 0, col = achrocol, radius = achrosize, lit = FALSE)
  }

  rgl::spheres3d(tcsdata[, c("x", "y", "z")],
    radius = size, color = col, alpha = alpha, lit = FALSE
  )

  if (floor) {
    vertices <- c(
      -0.7, -0.5, -0.3, 1.0,
      0.7, -0.5, -0.3, 1.0,
      0.7, 1, -0.3, 1.0,
      -0.7, 1, -0.3, 1.0
    )
    indices <- c(1, 2, 3, 4)

    rgl::wire3d(rgl::qmesh3d(vertices, indices), lit = FALSE)
  }

  if (gamut) {
    maxgamut <- attr(tcsdata, "data.maxgamut")
    colnames(maxgamut) <- c("x", "y", "z")
    attr(maxgamut, "clrsp") <- "tcs"
    tryCatch(tcsvol(maxgamut, grid = FALSE),
             error = function(e) warning("Max gamut cannot be plotted.",
                                         call. = FALSE))
  }

  if (hspin) {
    rgl::play3d(rgl::spin3d(axis = c(0, 0, 1), rpm = 20), duration = 3)
  }

  if (vspin) {
    rgl::play3d(rgl::spin3d(axis = c(1, 0, 0), rpm = 20), duration = 3)
  }
}
