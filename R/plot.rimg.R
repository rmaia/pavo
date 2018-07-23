#' Plot unprocessed or colour-classified images
#'
#' Plot unprocessed or colour-classified image data. If the
#' images are in a list, they will be stepped through
#' one by one.
#'
#' @param x (required) an image of class rimg, or list thereof.
#' @param axes should axes be drawn? (defaults to \code{TRUE})
#' @param col optional vector of colours when plotting colour-classified images.
#' Defaults to the mean RGB values of the k-means centres (i.e. the 'original' colours).
#' @param ... additional graphical parameters. Also see \code{\link{par}}.
#'
#' @return a image plot or plots.
#'
#' @export
#'
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' plot(papilio)
#' papilio_class <- classify(papilio, kcols = 4)
#' plot(papilio_class)
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' plot(snakes)
#' snakes_class <- classify(snakes, kcols = 3)
#' plot(snakes_class)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

plot.rimg <- function(x, axes = TRUE, col = NULL, ...) {
  multi_image <- inherits(x, "list") # Single or multiple images?

  if (!multi_image) {
    if (attr(x, "state") == "raw") {
      defaultrasterImageplot(x, axes = axes, ...)
    } else if (attr(x, "state") == "colclass") {
      defaultimageplot(x, axes = axes, col = col, ...)
    }
  } else if (multi_image) {
    if (attr(x[[1]], "state") == "raw") {
      for (i in 1:length(x)) {
        readline(prompt = "Press [enter] for next plot")
        defaultrasterImageplot(x[[i]], axes = axes, ...)
      }
    } else if (attr(x[[1]], "state") == "colclass") {
      for (i in 1:length(x)) {
        readline(prompt = "Press [enter] for next plot")
        defaultimageplot(x[[i]], axes = axes, col = col, ...)
      }
    }
  }
}

## For raw images
#' @import imager
#' @importFrom grDevices as.raster
#' @importFrom graphics box plot.new plot.window
defaultrasterImageplot <- function(imagedata, axes, ...) {
  if (missing(axes)) axes <- TRUE

  imagedata2 <- suppressWarnings(as.raster(as.cimg(imagedata, cc = 3)))

  # Defaults
  arg <- list(...)
  if (is.null(arg$xlab)) arg$xlab <- "x"
  if (is.null(arg$ylab)) arg$ylab <- "y"
  if (is.null(arg$main)) arg$main <- attr(imagedata, "imgname")
  if (is.null(arg$asp)) arg$asp <- dim(imagedata2)[1] / dim(imagedata2)[2]
  if (is.null(arg$xlim)) arg$xlim <- c(1, dim(imagedata2)[2])
  if (is.null(arg$ylim)) arg$ylim <- c(dim(imagedata2)[1], 1)
  if (is.null(arg$xaxs)) arg$xaxs <- "i"
  if (is.null(arg$yaxs)) arg$yaxs <- "i"

  plot.new()
  do.call(plot.window, arg)
  if (axes) {
    axis(1)
    axis(2)
    box()
  }
  title(arg$main, xlab = arg$xlab, ylab = arg$ylab)
  rasterImage(imagedata2, 1, nrow(imagedata2), ncol(imagedata2), 1)
}

## For colour-classified images
defaultimageplot <- function(rawimage, axes, col = NULL, ...) {

  # Transform to present the damn correct orientation
  imagedata <- as.matrix(t(apply(rawimage, 2, rev)))

  # Defaults
  arg <- list(...)
  if (is.null(arg$xlab)) arg$xlab <- "x"
  if (is.null(arg$ylab)) arg$ylab <- "y"
  if (is.null(arg$main)) arg$main <- attr(rawimage, "imgname")
  if (is.null(arg$asp)) arg$asp <- ncol(imagedata) / nrow(imagedata)
  if (is.null(arg$xlim)) arg$xlim <- c(1, nrow(imagedata))
  if (is.null(arg$ylim)) arg$ylim <- c(ncol(imagedata), 1)
  if (is.null(col)) col <- rgb(attr(rawimage, "classRGB"))

  plot.new()
  do.call(plot.window, arg)
  if (axes) {
    axis(1)
    axis(2)
    box()
  }
  title(arg$main, xlab = arg$xlab, ylab = arg$ylab)
  image(1:nrow(imagedata), 1:ncol(imagedata), imagedata, add = TRUE, useRaster = TRUE, col = col)
}
