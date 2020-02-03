#' Title
#'
#' @param vismodeldata the result from [vismodel()]. Quantum catches computed
#'   with another tool are not yet supported.
#' @param colour.solid logical (defaults to `TRUE`). Should the colour solid for
#'   the relevant illuminant and visual system be plotted.
#' @param ... additional arguments passed to [plot()].
#'
#' @return
#'
#' @export
#'
#' @details
#' This function only works with non-relative quantum catches
#' (`relative = FALSE` in [vismodel()]) as it would always be the line y = 1 -x
#' otherwise.
#'
#' @examples
#' data(sicalis)
#' vis_sicalis <- vismodel(sicalis, "canis", relative = FALSE)
#' plot(vis_sicalis)
#'
#' @references Wilkins, L., & Osorio D. C. (2019). Object colours, material
#'   properties and animal signals. Journal of Experimental Biologyn 222(21),
#'   \doi{10.1242/jeb.204487}.
#'
plot.vismodel <- function(vismodeldata, colour.solid = TRUE, ...) {

  if (!inherits(vismodeldata, "vismodel")) {
    stop("'vismodeldata' must be the output from the vismodel() function.")
  }
  if (attr(vismodeldata, "conenumb") != 2) {
    stop("Currently not implemented for anything else than dichromats.")
  }
  if (attr(vismodeldata, "relative")) {
    stop("plot.vismodel() only works with non-relative quantum catches")
  }

  maxqcatches <- as.data.frame(attr(vismodeldata, "data.maxqcatches"))
  solid1 <- cumsum(maxqcatches[order(maxqcatches$s / maxqcatches$l), ])
  solid2 <- cumsum(maxqcatches[order(maxqcatches$l / maxqcatches$s), ])
  solid <- rbind(solid1, solid2[rev(seq_len(nrow(solid2))), ], c(0, 0))

  arg <- list(...)

  if (is.null(arg$xlab)) {
    arg$xlab <- "s"
  }
  if (is.null(arg$ylab)) {
    arg$ylab <- "l"
  }
  if (is.null(arg$pch)) {
    arg$pch <- 19
  }
  if (is.null(arg$xlim)) {
    if(colour.solid) {
      arg$xlim <- c(0, 1)
    } else {
      arg$xlim <- c(0, 1.1 * max(vismodeldata$s))
    }
  }
  if (is.null(arg$ylim)) {
    if(colour.solid) {
      arg$ylim <- c(0, 1)
    } else {
      arg$ylim <- c(0, 1.1 * max(vismodeldata$l))
    }
  }

  do.call(plot, c(list(x = NULL), arg))

  if (colour.solid) {
    polygon(solid, col = "grey", border = NA)
  }

  arg$x <- vismodeldata$s
  arg$y <- vismodeldata$l

  do.call(points, arg)

}
