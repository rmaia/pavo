#' Plot reference axes in a static tetrahedral colourspace
#'
#' Plots reference x, y and z arrows showing the direction of
#' the axes in a static tetrahedral colourspace plot.
#'
#' @param x,y position of the legend relative to plot limits
#'  (usually a value between 0 and 1, but because of the perspective distortion,
#' values greater than 1 are possible)
#' @param size length of the arrows. Can be either a single value
#' (applied for x, y and z) or a vector of 3 separate values for each axis.
#' @param arrowhead size of the arrowhead.
#' @param col,lty,lwd graphical parameters for the arrows.
#' @param label logical, include x, y and z labels (defaults to `TRUE`).
#' @param adj.label position adjustment for the labels. a list of 3 named objects
#' for x, y and z arrows, each with 2 values for x and y adjustment.
#' @param label.cex,label.col graphical parameters for the labels.
#'
#' @return `axistetra` adds reference arrows showing the direction of the
#' 3-dimensional axes in a static tetrahedral colourspace plot.
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export
#'
#' @examples
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' plot(tcs.sicalis)
#' axistetra()

axistetra <- function(x = 0, y = 1.3, size = 0.1,
                      arrowhead = 0.05, col = par("fg"),
                      lty = par("lty"), lwd = par("lwd"),
                      label = TRUE,
                      adj.label = list(
                        x = c(0.003, 0),
                        y = c(0.003, 0.003),
                        z = c(0, 0.003)
                      ),
                      label.cex = 1, label.col = NULL) {
  if (length(size) > 1) {
    lx <- size[1]
    ly <- size[2]
    lz <- size[3]
  } else {
    lx <- ly <- lz <- size[1]
  }

  last_tetraplot <- get("last_plot.tetra", envir = .PlotTetraEnv)

  # turn relative coordinates into real coordinates
  xpos <- grconvertX(x, "npc")
  ypos <- grconvertY(y, "npc")

  # get arrow lengths
  xarr <- trans3d(c(0, lx), c(0, 0), c(0, 0), last_tetraplot)
  yarr <- trans3d(c(0, 0), c(0, ly), c(0, 0), last_tetraplot)
  zarr <- trans3d(c(0, 0), c(0, 0), c(0, lz), last_tetraplot)

  # add lengths to positions
  xarr$x <- xarr$x + xpos
  xarr$y <- xarr$y + ypos
  yarr$x <- yarr$x + xpos
  yarr$y <- yarr$y + ypos
  zarr$x <- zarr$x + xpos
  zarr$y <- zarr$y + ypos

  arrows(xarr$x[1], xarr$y[1], xarr$x[2], xarr$y[2],
    length = arrowhead,
    lty = lty, lwd = lwd, col = col
  )
  arrows(yarr$x[1], yarr$y[1], yarr$x[2], yarr$y[2],
    length = arrowhead,
    lty = lty, lwd = lwd, col = col
  )
  arrows(zarr$x[1], zarr$y[1], zarr$x[2], zarr$y[2],
    length = arrowhead,
    lty = lty, lwd = lwd, col = col
  )

  if (label) {
    px <- c(xarr$x[2] + adj.label$x[1], xarr$y[2] + adj.label$x[2])
    py <- c(yarr$x[2] + adj.label$y[1] / sqrt(2), yarr$y[2] + adj.label$y[2] / sqrt(2))
    pz <- c(zarr$x[2] + adj.label$z[1], zarr$y[2] + adj.label$z[2])

    text(px[1], px[2], labels = "x", cex = label.cex, col = label.col)
    text(py[1], py[2], labels = "y", cex = label.cex, col = label.col)
    text(pz[1], pz[2], labels = "z", cex = label.cex, col = label.col)
  }
}
