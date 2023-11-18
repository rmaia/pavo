#' Plot unprocessed or colour-classified images
#'
#' Plot unprocessed or colour-classified image data. If the
#' images are in a list, they will be stepped through
#' one by one.
#'
#' @param x (required) an image of class rimg, or list thereof.
#' @param axes should axes be drawn? (defaults to `TRUE`)
#' @param col optional vector of colours when plotting colour-classified images.
#' Defaults to the mean RGB values of the k-means centres (i.e. the average 'original'
#' colours).
#' @param ... additional graphical parameters. Also see [par()].
#'
#' @return an image plot.
#'
#' @importFrom graphics axis box title
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
#' plot(papilio)
#' \donttest{
#' papilio_class <- classify(papilio, kcols = 4)
#' plot(papilio_class)
#' }
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#' plot(snakes)
#' \donttest{
#' snakes_class <- classify(snakes, kcols = 3)
#' plot(snakes_class)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

plot.rimg <- function(x, axes = TRUE, col = NULL, ...) {
  multi_image <- inherits(x, "list") # Single or multiple images?

  if (multi_image) {
    for (i in seq_along(x)) {
      readline(prompt = "Press [enter] for next plot")
      defaultrasterImageplot(x[[i]], axes = axes, col = col, ...)
    }
  } else {
    defaultrasterImageplot(x, axes = axes, col = col, ...)
  }
}

## For raw images
#' @importFrom grDevices as.raster col2rgb
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

    mapR <- setNames(rgbs$R, seq_len(nrow(rgbs)))
    mapG <- setNames(rgbs$G, seq_len(nrow(rgbs)))
    mapB <- setNames(rgbs$B, seq_len(nrow(rgbs)))
    R <- matrix(mapR[img], nrow = nrow(img), dimnames = dimnames(img))
    G <- matrix(mapG[img], nrow = nrow(img), dimnames = dimnames(img))
    B <- matrix(mapB[img], nrow = nrow(img), dimnames = dimnames(img))

    imageout <- array(c(R, G, B), dim = c(dim(img)[1], dim(img)[2], 3))

    # Convert and transform
    imagedata2 <- suppressWarnings(as.raster(imageout))
    imagedata2 <- mirrorx(imagedata2)
    imagedata2 <- apply(t(as.matrix(imagedata2)), 2, rev)
  } else if (attr(imagedata, "state") == "raw") {
    # Convert and transform
    imagedata2 <- as.rimg(imagedata)
    imagedata2 <- mirrorx(imagedata2)
    imagedata2 <- rot90(imagedata2)
    imagedata2 <- suppressWarnings(as.raster(imagedata2))
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

## Rotate matrices 90-degrees
rot90 <- function(x) {
  permVec <- c(2, 1, 3:length(dim(x)))
  rotA <- aperm(x, permVec)
  rotA <- rotA[seq(dim(x)[2], 1), , ]
  rotA
}

## Mirror matrices about x axis
mirrorx <- function(x) {
  if (length(dim(x)) == 3) {
    for (i in seq_len(dim(x)[3])) {
      x[, , i] <- x[, , i][, rev(seq_len(ncol(x[, , i])))]
    }
  } else {
    x <- x[, rev(seq_len(ncol(x)))]
  }
  x
}
