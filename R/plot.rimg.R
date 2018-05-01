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
#' snakes_class <- classify(snakes, n_cols = 3)
#' plot(snakes_class)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

plot.rimg <- function(x, ...) {
  #  multi_image <- inherits(x, "list") # Single or multiple images?

  #  if (!isTRUE(multi_image)) {
  space <- attr(x, "state")
  #  } else if (isTRUE(multi_image)) {
  #   space <- attr(x[[1]], "state")
  # }

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
rawplot <- function(x, ...) {

  ## Checks
  multi_image <- inherits(x, "list") # Single or multiple images?

  if (isTRUE(multi_image)) { # Multiple images

    for (i in 1:length(x)) {
      readline(prompt = "Press [enter] for next plot")
      x2 <- x[[i]]

      # Defaults
      arg <- list(...)

      if (is.null(arg$xlab)) {
        arg$xlab <- "x"
      }
      if (is.null(arg$ylab)) {
        arg$ylab <- "y"
      }
      if (is.null(arg$type)) {
        arg$type <- "n"
      }
      if (is.null(arg$asp)) {
        arg$asp <- dim(x2)[1] / dim(x2)[2]
      }

      arg$x <- c(1, dim(x2)[2])
      arg$y <- c(1, dim(x2)[1])

      do.call(graphics::plot, arg)
      graphics::rasterImage(x2, 1, 1, dim(x2)[2], dim(x2)[1])
    }
  } else if (!isTRUE(multi_image)) { # Single image
    # Defaults
    arg <- list(...)

    if (is.null(arg$xlab)) {
      arg$xlab <- "x"
    }
    if (is.null(arg$ylab)) {
      arg$ylab <- "y"
    }
    if (is.null(arg$type)) {
      arg$type <- "n"
    }
    if (is.null(arg$asp)) {
      arg$asp <- dim(x)[1] / dim(x)[2]
    }

    arg$x <- c(1, dim(x)[2])
    arg$y <- c(1, dim(x)[1])

    do.call(graphics::plot, arg)
    graphics::rasterImage(x, 1, 1, dim(x)[2], dim(x)[1])
  }
}

#' Plot colour-classified images
#'
#' @param x (required) colour-classified image data, or a list thereof. Preferably
#' the result of \code{\link{classify}}.
#' @param x_pts an optional integer specifying the number of sample points (grid
#' sampling density) along the x axis, with the resulting sampling grid being
#' plotted atop the sample image. Useful for planning the sampling density prior
#' to adjacency analysis .
#' @param aspect logical; Should the aspect ratio of the original image be replicated?
#' Defaults to \code{TRUE}.
#' @param grid.col the colour of sampling-grid points.
#' @param grid.cex the size of sampling-grid points.
#' @param ... additional graphical parameters passed to \code{\link{image}}.
#'
#' @keywords internal
#'
classplot <- function(x, x_pts = NULL, aspect = TRUE, grid.col = "red", grid.cex = 1, ...) {

  ## Checks
  multi_image <- inherits(x, "list") # Single or multiple images?

  # Reformat & rotate to account for the silliness of image()
  if (isTRUE(multi_image)) {
    x_trans <- lapply(1:length(x), function(y) as.matrix(t(apply(x[[y]], 2, rev))))
  } else if (!isTRUE(multi_image)) {
    imgdat2 <- as.matrix(t(apply(x, 2, rev)))
  }

  if (isTRUE(multi_image)) { # Multiple images

    for (i in 1:length(x)) {
      readline(prompt = "Press [enter] for next plot")
      imgdat2 <- x_trans[[i]]

      # Defaults
      arg <- list(...)

      if (is.null(arg$xlab)) {
        arg$xlab <- "x"
      }
      if (is.null(arg$ylab)) {
        arg$ylab <- "y"
      }
      if (is.null(arg$xlim)) {
        padrow <- round(nrow(imgdat2) * 0.02)
        arg$xlim <- c(0 - padrow, nrow(imgdat2) + padrow)
      }
      if (is.null(arg$ylim)) {
        padcol <- round(ncol(imgdat2) * 0.02)
        arg$ylim <- c(0 - padcol, ncol(imgdat2) + padcol)
      }
      if (isTRUE(aspect)) {
        arg$asp <- dim(imgdat2)[1] / dim(imgdat2)[2]
      }
      if (is.null(arg$useRaster)) {
        arg$useRaster <- TRUE
      }
      if (is.null(arg$col)) {
        values <- attr(x[[i]], "classRGB")
        arg$col <- grDevices::rgb(values)
      }

      # Main plot
      arg$x <- 1:nrow(imgdat2)
      arg$y <- 1:ncol(imgdat2)
      arg$z <- imgdat2

      do.call(graphics::image, arg)

      # Visualise the sampling grid
      if (!is.null(x_pts)) {
        grid <- expand.grid(
          seq(from = 1, to = nrow(imgdat2), by = nrow(imgdat2) / x_pts),
          seq(from = 1, to = ncol(imgdat2), by = ncol(imgdat2) / x_pts)
        )

        names(grid) <- c("y", "x")
        graphics::points(grid, col = grid.col, pch = 16, cex = grid.cex)
      }
    }
  } else if (!isTRUE(multi_image)) { # Single image

    # Defaults
    arg <- list(...)

    if (is.null(arg$xlab)) {
      arg$xlab <- "x"
    }
    if (is.null(arg$ylab)) {
      arg$ylab <- "y"
    }
    if (is.null(arg$xlim)) {
      padrow <- round(nrow(imgdat2) * 0.02)
      arg$xlim <- c(0 - padrow, nrow(imgdat2) + padrow)
    }
    if (is.null(arg$ylim)) {
      padcol <- round(ncol(imgdat2) * 0.02)
      arg$ylim <- c(0 - padcol, ncol(imgdat2) + padcol)
    }
    if (isTRUE(aspect)) {
      arg$asp <- dim(x)[1] / dim(x)[2]
    }
    if (is.null(arg$useRaster)) {
      arg$useRaster <- TRUE
    }
    if (is.null(arg$col)) {
      values <- attr(x, "classRGB")
      arg$col <- grDevices::rgb(values)
    }

    # Main plot
    arg$x <- 1:nrow(imgdat2)
    arg$y <- 1:ncol(imgdat2)
    arg$z <- imgdat2

    do.call(graphics::image, arg)

    # Visualise the sampling grid
    if (!is.null(x_pts)) {
      grid <- expand.grid(
        seq(from = 1, to = nrow(imgdat2), by = nrow(imgdat2) / x_pts),
        seq(from = 1, to = ncol(imgdat2), by = ncol(imgdat2) / x_pts)
      )

      names(grid) <- c("y", "x")
      graphics::points(grid, col = grid.col, pch = 16, cex = grid.cex)
    }
  }
}
