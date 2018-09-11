#' Plot unprocessed or colour-classified images
#'
#' Plot unprocessed or colour-classified image data. If the
#' images are in a list, they will be stepped through
#' one by one.
#'
#' @param x (required) an image of class rimg, or list thereof.
#' @param axes should axes be drawn? (defaults to \code{TRUE})
#' @param col optional vector of colours when plotting colour-classified images.
#' Defaults to the mean RGB values of the k-means centres (i.e. the average 'original' 
#' colours).
#' @param ... additional graphical parameters. Also see \code{\link{par}}.
#'
#' @return a image plot.
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
    defaultrasterImageplot(x, axes = axes, col = col, ...)
  } else {
    for (i in 1:length(x)) {
      readline(prompt = "Press [enter] for next plot")
      defaultrasterImageplot(x[[i]], axes = axes, col = col, ...)
    }
  }
}

## For raw images
#' @import imager
#' @importFrom grDevices as.raster
#' @importFrom graphics box plot.new plot.window
defaultrasterImageplot <- function(imagedata, axes, col, ...) {
  if (missing(axes)) axes <- TRUE

  if (attr(imagedata, "state") == "colclass") {

    # Transform again
    img3 <- rev(t(apply(imagedata, 1, rev))) # mirror
    dim(img3) <- dim(imagedata)
    img <- t(apply(img3, 2, rev)) # rotate 90

    # Reconstitute image
    if (!is.null(col)) {
      rgbs <- as.data.frame(t(col2rgb(col) / 255))
      names(rgbs) <- c("R", "G", "B")
    } else {
      rgbs <- attr(imagedata, "classRGB")
    }

    mapR <- setNames(rgbs$R, 1:nrow(rgbs))
    mapG <- setNames(rgbs$G, 1:nrow(rgbs))
    mapB <- setNames(rgbs$B, 1:nrow(rgbs))
    R <- matrix(mapR[img], nrow = nrow(img), dimnames = dimnames(img))
    G <- matrix(mapG[img], nrow = nrow(img), dimnames = dimnames(img))
    B <- matrix(mapB[img], nrow = nrow(img), dimnames = dimnames(img))

    imageout <- array(c(R, G, B), dim = c(dim(img)[1], dim(img)[2], 3))

    imagedata2 <- suppressWarnings(as.raster(as.cimg(imageout, cc = 3)))
  } else if (attr(imagedata, "state") == "raw") {
    imagedata2 <- suppressWarnings(as.raster(as.cimg(imagedata, cc = 3)))
  }

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
