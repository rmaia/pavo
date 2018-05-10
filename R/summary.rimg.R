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
#' @examples \dontrun{
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, n_cols = 4)
#' summary(papilio_class)
#'
#' # Plot the colour-classified image alongside the colour class palette
#' summary(papilio_class, plot = TRUE)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

summary.rimg <- function(object, plot = FALSE, ...) {
  
  #values <- attr(object, "classRGB")
  
  if (isTRUE(plot)) {
    
    object2 <- as.matrix(t(apply(object, 2, rev)))
    
    # Defaults for image plot
    arg <- list(...)
    
    if (is.null(arg$xlab)) {
      arg$xlab <- "x"
    }
    if (is.null(arg$ylab)) {
      arg$ylab <- "y"
    }
    if (is.null(arg$xlim)) {
      padrow <- round(nrow(object2) * 0.02)
      arg$xlim <- c(0 - padrow, nrow(object2) + padrow)
    }
    if (is.null(arg$ylim)) {
      padcol <- round(ncol(object2) * 0.02)
      arg$ylim <- c(0 - padcol, ncol(object2) + padcol)
    }
    if (is.null(arg$asp)) {
      arg$asp <- dim(object)[1] / dim(object)[2]
    }
    if (is.null(arg$useRaster)) {
      arg$useRaster <- TRUE
    }
    if (is.null(arg$col)) {
      #values <- attr(object, "classRGB")
      arg$col <- rgb(attr(object, "classRGB"))
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
