#' Add legend to a static tetrahedral colourspace
#'
#' Adds a legend to a static tetrahedral colourspace plot.
#'
#' @inheritParams axistetra
#' @param ... additional arguments passed to [legend()].
#'
#' @return [legendtetra()] adds a legend to a static tetrahedral colourspace plot.
#' for additional information on which arguments are necessary and how they are used,
#' see [legend()].
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @examples
#' data(sicalis)
#'
#' vis_sicalis <- vismodel(sicalis)
#' tcs_sicalis <- colspace(vis_sicalis)
#'
#' cols <- c("#1B9E77", "#D95F02", "#7570B3")
#' plot(tcs_sicalis, col = cols)
#' legendtetra(
#'   legend = c("Crown", "Throat", "Breast"),
#'   col = cols, pch = 16
#' )
#' @export

legendtetra <- function(x = 0.8, y = 1.2, ...) {
  arg <- list(...)

  arg$x <- grconvertX(x, "npc")
  arg$y <- grconvertY(y, "npc")

  do.call(legend, arg)
}
