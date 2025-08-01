#' Colour hexagon
#'
#' Calculates coordinates and colourimetric variables that represent reflectance spectra
#' in the hymenopteran colour hexagon.
#'
#' @inheritParams trispace
#'
#' @return A data frame of class `colspace` consisting of the following columns:
#' * `s`, `m`, `l`: the quantum catch data used to calculate
#'  the remaining variables
#' * `x`, `y`: cartesian coordinates in the colour hexagon.
#' * `h.theta`: hue angle theta (in degrees), with 0-degrees at the 1200
#'  angle, increasing clockwise.
#' * `r.vec`: the r vector (saturation, distance from the center).
#' * `sec.fine`: fine 'hue sector', wherein the full hexagon is composed
#'  of 36 10-degree sectors, with 0-degrees at the 1200 angle.
#' * `sec.coarse`: coarse 'hue sector', wherein the full hexagon is
#'  composed of five sectors: blue, bluegreen, green, uvgreen, uv, and uvblue.
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers,
#'   visual = "apis", qcatch = "Ei", relative = FALSE,
#'   vonkries = TRUE, achromatic = "l", bkg = "green"
#' )
#' flowers.hex <- colspace(vis.flowers, space = "hexagon")
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @keywords internal
#'
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'    based on photoreceptor excitations as a generalized representation of
#'    colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Chittka L, Shmida A, Troje N, Menzel R. (1994). Ultraviolet as a
#'    component of flower reflections, and the colour perception of Hymenoptera.
#'    Vision research, 34(11), 1489-1508.

hexagon <- function(vismodeldata) {
  ## Note: requires von kries & hyperbolic transform

  if (is.vismodel(vismodeldata)) {
    # check if relative. Qcatches, at this stage, need to be raw & not log-transformed
    # for the hexagon as it uses a hyperbolic transform.
    if (attr(vismodeldata, "relative")) {
      stop("Quantum catches are relative, which is not required in the hexagon model.", call. = FALSE)
    }
    if (attr(vismodeldata, "qcatch") != "Ei") {
      warning("Quantum catches are not hyperbolically transformed, as required for the hexagon model. This may produce unexpected results.", call. = FALSE)
    }
    if (!isTRUE(attr(vismodeldata, "vonkries"))) {
      warning("Quantum catches are not von-Kries transformed, as required for the hexagon model. This may produce unexpected results.", call. = FALSE)
    }
  }

  dat <- check_data_for_colspace(
    vismodeldata,
    c("s", "m", "l"),
    force_relative = FALSE
  )

  if (isTRUE(all.equal(rowSums(dat), rep(1, nrow(dat)), check.attributes = FALSE))) {
    stop("Quantum catches are relative, which is not required in the hexagon model.", call. = FALSE)
  }

  # Hexagon coordinates & colorimetrics
  x <- (sqrt(3) / 2) * (dat$l - dat$s)
  y <- dat$m - (0.5 * (dat$s + dat$l))

  # colorimetrics
  r.vec <- sqrt(x^2 + y^2)
  h.theta <- vapply(seq_along(x), function(i) angle360(x[i], y[i]), numeric(1))
  sec.fine <- round(floor(h.theta / 10), 0) * 10
  sec.coarse <- vapply(seq_along(x), function(x) coarse_sec(h.theta[x]), character(1))

  res <- data.frame(dat, x, y, h.theta, r.vec, sec.fine, sec.coarse,
    row.names = rownames(dat),
    stringsAsFactors = FALSE
  )
  # res.p <- data.frame(s, m, l, x, y, r.vec, row.names = rownames(dat))

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "hexagon"
  attr(res, "conenumb") <- 3
  attr(res, "qcatch") <- attr(vismodeldata, "qcatch")
  attr(res, "visualsystem.chromatic") <- attr(vismodeldata, "visualsystem.chromatic")
  attr(res, "visualsystem.achromatic") <- attr(vismodeldata, "visualsystem.achromatic")
  attr(res, "illuminant") <- attr(vismodeldata, "illuminant")
  attr(res, "background") <- attr(vismodeldata, "background")
  attr(res, "relative") <- attr(vismodeldata, "relative")
  attr(res, "vonkries") <- attr(vismodeldata, "vonkries")

  # Data attributes
  attr(res, "data.visualsystem.chromatic") <- attr(vismodeldata, "data.visualsystem.chromatic")
  attr(res, "data.visualsystem.achromatic") <- attr(vismodeldata, "data.visualsystem.achromatic")
  attr(res, "data.background") <- attr(vismodeldata, "data.background")

  res
}
