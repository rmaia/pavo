#' Colour-classified image summary
#'
#' Returns the attributes of the colour-classified matrix generated from \code{\link{classify}}.
#'
#' @param object (required) Results of \code{\link{classify}}.
#' @param plot logical; plot both the image and the colours corresponding to colour class
#' categories side-by-side? Defaults to \code{FALSE}.
#' @param ... additional graphical options when \code{plot = TRUE}. Also see \code{\link{par}}.
#'
#' @return Either the RGB values of the k-means centres from the colour-classified image,
#' or a plot of both the image and specified colours (when \code{plot = TRUE}.
#'
#' @export
#'
#' @importFrom graphics image
#' @importFrom grDevices rgb
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, n_cols = 4)
#' summary(papilio_class)
#'
#' # Plot the colour-classified image alongside the colour class palette
#' summary(papilio_class, plot = TRUE)
#' }
#'

summary.rimg <- function(object, plot = FALSE, ...) {
  multi_image <- inherits(object, "list") # Single or multiple images?

  if (multi_image) {
    if (plot) {
      for (i in 1:length(object)) {
        readline(prompt = "Press [enter] for next plot.")
        summary_main(object[[i]], plot, ...)
      }
    } else {
      out <- lapply(1:length(object), function(x) data.frame(
          ID = attr(object[[x]], "imgname"),
          col_ID = seq(1:nrow(attr(object[[x]], "classRGB"))),
          attr(object[[x]], "classRGB")
        ))
      do.call(rbind, out)
    }
  } else if (!multi_image) {
    if (plot) {
      summary_main(object, plot, ...)
    } else {
      data.frame(
        ID = attr(object, "imgname"),
        col_ID = seq(1:nrow(attr(object, "classRGB"))),
        attr(object, "classRGB")
      )
    }
  }
}

summary_main <- function(object, plot, ...) {
  if (plot) {
    
    object2 <- as.matrix(t(apply(object, 2, rev)))

    # Defaults for image plot
    arg <- list(...)
    if (is.null(arg$xlab)) arg$xlab <- "x"
    if (is.null(arg$ylab)) arg$ylab <- "y"
    if (is.null(arg$main)) arg$main <- attr(object, "imgname")
    if (is.null(arg$useRaster)) arg$useRaster <- TRUE
    if (is.null(arg$col)) arg$col <- rgb(attr(object, "classRGB"))
    if (is.null(arg$asp)) arg$asp <- dim(object)[1] / dim(object)[2]
    if (is.null(arg$xlim)) {
      padrow <- round(nrow(object2) * 0.02)
      arg$xlim <- c(0 - padrow, nrow(object2) + padrow)
    }
    if (is.null(arg$ylim)) {
      padcol <- round(ncol(object2) * 0.02)
      arg$ylim <- c(0 - padcol, ncol(object2) + padcol)
    }

    arg$x <- 1:nrow(object2)
    arg$y <- 1:ncol(object2)
    arg$z <- object2

    # Plotting
    par(mfrow = c(1, 2))
    on.exit(par(mfrow = c(1, 1)))

    # Main
    do.call(image, arg)

    # Palette
    image(1:length(arg$col), 1, as.matrix(1:length(arg$col)),
      col = arg$col,
      xlab = paste("Colour class IDs:", paste(1:length(arg$col), collapse = ", ")), ylab = "", xaxt = "n", yaxt = "n"
    )
  } else {
    attr(object, "classRGB")
  }
}