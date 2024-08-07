#' Plot the categorical colour vision model
#'
#' Produces a plot based on Troje's (1993) categorical colour model.
#'
#' @param catdata (required) a data frame, possibly a result from the
#'   [colspace()] or [categorical()] function, containing values for 'x' and 'y'
#'   coordinates as columns (labeled as such).
#' @param labels plot category labels inside? Defaults to `TRUE`.
#' @inheritParams triplot
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, qcatch = "Qi", visual = "musca", achro = "none", relative = TRUE)
#' cat.flowers <- colspace(vis.flowers, space = "categorical")
#' plot(cat.flowers)
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @importFrom graphics abline legend
#'
#' @export
#'
#' @keywords internal
#'
#' @inherit categorical references

catplot <- function(catdata, labels = TRUE, labels.cex = 0.9, ...) {
  # Check if object is of class colorspace and tetrachromat
  if (!is.colspace(catdata) && !all(c("x", "y") %in% names(catdata))) {
    stop(
      "object is not of class ", dQuote("colspace"),
      ", and does not contain x, y coordinates",
      call. = FALSE
    )
  }

  if (is.colspace(catdata) && attr(catdata, "clrsp") != "categorical") {
    stop(dQuote("colspace"), " object is not a result of categorical()", call. = FALSE)
  }

  # Set defaults
  defaults <- list(
    pch = 19,
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    xlab = "R7p - R8p",
    ylab = "R7y - R8y"
  )
  arg <- modifyList(
    defaults,
    list(...)
  )

  # Plot
  arg$x <- catdata$x
  arg$y <- catdata$y

  do.call(plot, arg)
  abline(h = 0, v = 0, col = "grey") # Divide up categories

  # Category labels (todo: make this more flexible/robust?)
  if (labels) {
    legend(x = "topleft", legend = "p- y+", bty = "n", cex = labels.cex)
    legend(x = "topright", legend = "p+ y+", bty = "n", cex = labels.cex)
    legend(x = "bottomleft", legend = "p- y-", bty = "n", cex = labels.cex)
    legend(x = "bottomright", legend = "p+ y-", bty = "n", cex = labels.cex)
  }
}
