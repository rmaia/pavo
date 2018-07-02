#' Plot unprocessed or colour-classified images
#'
#' Plot unprocessed or colour-classified image data. If the
#' input data is a list of classified image data, images will be stepped through
#' one by one.
#'
#' @param x (required) unprocessed or colour-classified image data, or a list thereof.
#' Preferably the result of \code{\link{classify}}.
#' @param ... additional graphical parameters. See \code{\link{rawplot}} for unprocessed
#' image plotting, and \code{\link{classplot}} for colour-classified image plots.
#' Also see \code{\link{par}}.
#'
#' @return a image plot or plots.
#'
#' @export
#'
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' plot(papilio)
#' papilio_class <- classify(papilio, n_cols = 4)
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

plot.rimg <- function(x, ...) {
  multi_image <- inherits(x, "list") # Single or multiple images?

  if (!multi_image) {
    space <- attr(x, "state")
  } else if (multi_image) {
    space <- attr(x[[1]], "state")
  }

  switch(space,
    "raw" = rawplot(x, ...),
    "colclass" = classplot(x, ...)
  )
}

#' Plot unprocessed images
#'
#' @param x (required) colour-classified image data, or a list thereof. Preferably
#' the result of \code{\link{classify}}.
#' @param ... additional graphical parameters plassed to \code{\link{plot}}.
#'
#' @keywords internal
#'
#' @importFrom graphics plot rasterImage
#'
rawplot <- function(x, ...) {

  ## Checks
  multi_image <- inherits(x, "list") # Single or multiple images?

  if (multi_image) { # Multiple images
    for (i in 1:length(x)) {
      readline(prompt = "Press [enter] for next plot")
      defaultrasterImageplot(x[[i]], ...)
    }
  } else if (!multi_image) { # Single image
    defaultrasterImageplot(x, ...)
  }
}

#' Plot colour-classified images
#'
#' @param x (required) colour-classified image data, or a list thereof. Preferably
#' the result of \code{\link{classify}}.
#' @param ... additional graphical parameters passed to \code{\link{image}}.
#'
#' @keywords internal
#'
#' @importFrom grDevices rgb
#' @importFrom graphics image points
#'
classplot <- function(x, ...) {

  ## Checks
  multi_image <- inherits(x, "list") # Single or multiple images?

  if (multi_image) { # Multiple images
    for (i in 1:length(x)) {
      readline(prompt = "Press [enter] for next plot")
      defaultimageplot(x[[i]], ...)
    }
  } else if (!multi_image) { # Single image
    defaultimageplot(x, ...)
  }
}

## For colour-classified images
defaultimageplot <- function(rawimage, ...) {

  # Transform to present the damn correct orientation
  imagedata <- as.matrix(t(apply(rawimage, 2, rev))) 
  
  # Defaults
  arg <- list(...)
  if (is.null(arg$xlab)) arg$xlab <- "x"
  if (is.null(arg$ylab)) arg$ylab <- "y"
  if (is.null(arg$main)) arg$main <- attr(rawimage, "imgname")
  if (is.null(arg$asp)) arg$asp <- dim(rawimage)[1] / dim(rawimage)[2]
  if (is.null(arg$useRaster)) arg$useRaster <- TRUE
  if (is.null(arg$col)) arg$col <- rgb(attr(rawimage, "classRGB"))
  if (is.null(arg$xlim)) {
    padrow <- round(nrow(imagedata) * 0.02, 1)
    arg$xlim <- c(0 - padrow, nrow(imagedata) + padrow)
  }
  if (is.null(arg$ylim)) {
    padcol <- round(ncol(imagedata) * 0.02, 1)
    arg$ylim <- c(0 - padcol, ncol(imagedata) + padcol)
  }

  # Main plot
  arg$x <- 1:nrow(imagedata)
  arg$y <- 1:ncol(imagedata)
  arg$z <- imagedata

  do.call(image, arg)
}

## For raw images
defaultrasterImageplot <- function(imagedata, ...) {

  # Defaults
  arg <- list(...)
  if (is.null(arg$xlab)) arg$xlab <- "x"
  if (is.null(arg$ylab)) arg$ylab <- "y"
  if (is.null(arg$main)) arg$main <- attr(imagedata, "imgname")
  if (is.null(arg$type)) arg$type <- "n"
  if (is.null(arg$asp)) arg$asp <- dim(imagedata)[1] / dim(imagedata)[2]

  arg$x <- c(1, dim(imagedata)[2])
  arg$y <- c(1, dim(imagedata)[1])

  do.call(plot, arg)
  rasterImage(imagedata, 1, 1, dim(imagedata)[2], dim(imagedata)[1])
}
