#' Plot absorbance spectra from [sensmodel()]
#'
#' @inheritParams plot.rspec
#'
#' @examples
#' # Blue tit visual system based on Hart et al (2000)
#' bluesens <- sensmodel(c(371, 448, 502, 563),
#'   beta = FALSE,
#'   lambdacut = c(330, 413, 507, 572),
#'   oiltype = c("T", "C", "Y", "R"), om = TRUE
#' )
#' plot(bluesens)
#'
#' # Alternatively, you can specify your own ylab
#' plot(bluesens, ylab = "absor.")
#'
#' @seealso [plot.rspec()], [sensmodel()]
#'
#' @export

plot.sensmod <- function(x, select = NULL,
                         type = c("overlay", "stack", "heatmap"),
                         varying = NULL, n = 100, labels = FALSE,
                         labels.stack = NULL, labels.cex = 1,
                         wl.guide = TRUE, ...) {
  if ("ylab" %in% names(match.call()[-1])) {
    NextMethod()
  } else {
    plot.rspec(
      x, select, type, varying, n, labels, labels.stack, labels.cex, wl.guide,
      ylab = "Absorbance",
      ...
    )
  }
}
