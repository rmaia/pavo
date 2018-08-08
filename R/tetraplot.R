#' Plot a static tetrahedral colorspace
#'
#' Produces a static 3D tetrahedral plot.
#'
#'
#' @param tcsdata (required) a data frame, possibly a result from the \code{colspace}
#' or \code{tetraspace} function, containing values for the 'x', 'y' and 'z'
#' coordinates as columns (labeled as such).
#' @param theta angle to rotate the plot in the xy plane (defaults to 45).
#' @param phi angle to rotate the plot in the yz plane (defaults to 10).
#' @param perspective logical, should perspective be forced by using point size to reflect
#' distance from the plane of view? (defaults to FALSE)
#' @param range, vert.range multiplier applied to \code{cex} and \code{vert.cex}, respectively,
#' to indicate the size range variation reflecting the distance from the plane of view.
#' @param r the distance of the eyepoint from the center of the plotting box.
#' Very high values approximate an orthographic projection (defaults to 1e6).
#' See \code{\link{persp}} for details.
#' @param zoom zooms in (values greater than 1) or out (values between 0 and 1) from the plotting area.
#' @param achro logical. Should the achromatic center be plotted? (defaults to \code{TRUE})
#' @param achro.line logical. Should the achromatic line be plotted? (defaults to \code{FALSE})
#' @param achro.col,achro.size,achro.lwd,achro.lty graphical parameters for the achromatic coordinates.
#' @param tetrahedron logical. Should the tetrahedron be plotted? (defaults to \code{TRUE})
#' @param vert.cex size of the points at the vertices (defaults to 1).
#' @param out.lwd,out.lcol graphical parameters for the tetrahedral outline.
#' @param margin vector of four numbers specifying drawing margins (defaults to c(0, 0, 0, 0)).
#' @param type accepts a vector of length 1 or 2 with 'p' for points and/or 'l' for lines from the point to
#' the base of the tetrahedron.
#' @param labels logical. Should the name of each cone be printed next to the
#' corresponding vertex?
#'
#' @return \code{tetraplot} creates a 3D plot.
#'
#' @examples \dontrun{
#'
#' # For plotting
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'avg.uv')
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#' plot(tcs.sicalis)
#'
#' }
#'
#' @seealso \code{\link{plot.colspace}},\code{\link[plot3D]{scatter3D}},
#'
#' @author Hugo Gruson \email{hugo.gruson@@normalesup.org}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Chad Eliason \email{cme16@zips.uakron.edu}
#'
#' @export
#'
#' @keywords internal
#'
#' @import plot3D
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings.
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.

tetraplot <- function(tcsdata,
                      theta = 45, phi = 10,
                      perspective = FALSE, range = c(1, 2),
                      r = 1e6, zoom = 1,
                      achro = TRUE, achro.col = "grey", achro.size = 1,
                      achro.line = FALSE, achro.lwd = 1, achro.lty = 3,
                      tetrahedron = TRUE,
                      vert.cex = 1, vert.range = c(1, 2),
                      out.lwd = 1, out.lcol = "darkgrey",
                      marginal = FALSE,
                      margin = c(2, 2, 2, 2),
                      type = "p", ...) {

  space = 1

  par(mar = margin, pty = "s")

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

  pmat <- scatter3D(tcsdata$x, tcsdata$y, tcsdata$z,
                    colvar = NULL, pch = 16,
                    phi = phi, theta = theta,
                    xlim = range(verts[, 1]) + c(-space, 0),
                    ylim = range(verts[, 2]) + c(0, space),
                    zlim = range(verts[, 3]) + c(-space, 0),
                    r = r,
                    bty = "g", xlab = "", ylab = "", zlab = "")

  if (achro) {
    scatter3D(x = 0, y = 0, z = 0,
              col = achro.col, bg = achro.col, pch = 22, size = achro.size,
              add = TRUE)
  }

  if (achro.line) {
    lines3D(x = c(0, 0),
            y = c(0, 0),
            z = c(-0.25, 0.75),
            col = achro.col, lty = achro.lty, lwd = achro.lwd,
            add = TRUE)
  }

  if (tetrahedron) {
    lines3D(x = verts[c(1, 2, 3, 4, 1, 3, 1, 2, 4), 1],
            y = verts[c(1, 2, 3, 4, 1, 3, 1, 2, 4), 2],
            z = verts[c(1, 2, 3, 4, 1, 3, 1, 2, 4), 3],
            lwd = out.lwd, col = out.lcol,
            add = TRUE)
    scatter3D(x = verts[, 1],
              y = verts[, 2],
              z = verts[, 3],
              colvar = NULL,
              col = c("darkorchid1", "cornflowerblue", "mediumseagreen", "firebrick1"),
              colkey = FALSE,
              pch = 19,
              add = TRUE)
  }

  if (marginal) {
    YZ <- trans3D(x = rep(-space, nrow(tcsdata)),
                  y = tcsdata$y,
                  z = tcsdata$z,
                  pmat = pmat)
    scatter2D(YZ$x, YZ$y,
              col = "#E69F0055", pch = 16,
              add = TRUE)
    XZ <- trans3D(x = tcsdata$x,
                  y = rep(space, nrow(tcsdata)),
                  z = tcsdata$z,
                  pmat = pmat)
    scatter2D(XZ$x, XZ$y,
              col = "#56B4E955", pch = 16,
              add = TRUE)
    XY <- trans3D(x = tcsdata$x,
                  y = tcsdata$y,
                  z = rep(-space, nrow(tcsdata)),
                  pmat = pmat)
    scatter2D(XY$x, XY$y,
              col = "#009E7355", pch = 16,
              add = TRUE)
  }

#  assign("last_plot.tetra", pmat, envir = .PlotTetraEnv)

}
