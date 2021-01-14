#' Plot a Maxwell triangle
#'
#' Produces a Maxwell triangle plot.
#'
#' @param tridata (required) a data frame, possibly a result from the
#'   [colspace()] or [trispace()] function, containing values for the 'x' and
#'   'y' coordinates as columns (labeled as such).
#' @param achro should a point be plotted at the origin (defaults to `TRUE`)?
#' @param labels logical. Should the name of each cone be printed next to the
#'   corresponding vertex?
#' @param labels.cex  size of the arrow labels.
#' @param achrosize size of the point at the origin when `achro = TRUE`
#'   (defaults to `0.8`).
#' @param achrocol color of the point at the origin `achro = TRUE` (defaults to
#'   `'grey'`).
#' @param out.lwd,out.lcol,out.lty graphical parameters for the plot outline.
#' @param square logical. Should the aspect ratio of the plot be held to 1:1?
#'   (defaults to `TRUE`).
#' @param gamut logical. Should the polygon showing the possible colours given
#'   visual system and illuminant used in the analysis (defaults to `FALSE`).
#'   This option currently only works when `qcatch = Qi`.
#' @param margins Deprecated. Please use the standard par() method for custom margins. 
#' @param ... additional graphical options. See [par()].
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "apis")
#' tri.flowers <- colspace(vis.flowers, space = "tri")
#' plot(tri.flowers)
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @importFrom graphics par points polygon
#'
#' @export
#'
#' @keywords internal
#'
#' @inherit trispace references


triplot <- function(tridata, labels = TRUE, achro = TRUE, achrocol = "grey", achrosize = 0.8,
                    labels.cex = 1, out.lwd = 1, out.lcol = "black", out.lty = 1,
                    square = TRUE, gamut = FALSE, margins = NULL, ...) {

  if (!missing("margins"))
    message("The 'margins' argument is deprecated, and will be ignored. See ?par() for guidance on 
            setting margins in the standard manner.")

  arg <- list(...)

  # Set defaults
  if (square) {
    arg$asp <- 1
  }
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }
  if (is.null(arg$xlim)) {
    arg$xlim <- c(-1 / sqrt(2), 1 / sqrt(2))
  }
  if (is.null(arg$ylim)) {
    arg$ylim <- c(-sqrt(2) / (2 * (sqrt(3))), sqrt(2) / sqrt(3))
  }

  # Verticy coordinates
  vert <- data.frame(
    x = c(0, -1 / sqrt(2), 1 / sqrt(2)),
    y = c(sqrt(2) / sqrt(3), -sqrt(2) / (2 * (sqrt(3))), -sqrt(2) / (2 * (sqrt(3))))
  )

  # Plot
  arg$x <- tridata$x
  arg$y <- tridata$y
  arg$xlab <- ""
  arg$ylab <- ""
  arg$bty <- "n"
  arg$axes <- FALSE

  do.call(plot, c(arg, type = "n"))

  # Add lines
  polygon(vert, lwd = out.lwd, lty = out.lty, border = out.lcol)

  if (gamut) {
    coords_mono <- attr(tridata, "data.maxgamut")
    max_gamut <- tryCatch(
      {
        convhulln(coords_mono)
        polygon(coords_mono[sort(c(max_gamut)), ],
          col = "#55555555", border = NA
        )
      },
      error = function(e) warning("Max gamut cannot be plotted.", call. = FALSE)
    )
  }

  # Origin
  if (isTRUE(achro)) {
    points(x = 0, y = 0, pch = 15, col = achrocol, cex = achrosize)
  }

  # remove plot-specific args, add points after the stuff is drawn
  arg[c(
    "type", "xlim", "ylim", "log", "main", "sub", "xlab", "ylab",
    "ann", "axes", "frame.plot", "panel.first", "panel.last", "asp"
  )] <- NULL
  do.call(points, arg)


  # Add text (coloured points better as in tcsplot?)
  if (isTRUE(labels)) {
    text(vert,
      labels = c("S", "M", "L"),
      pos = c(3, 2, 4),
      xpd = TRUE,
      cex = labels.cex
    )
  }
}
