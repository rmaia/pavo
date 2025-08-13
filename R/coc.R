#' Color opponent coding model
#'
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in the color opponent coding model of hymenopteran vision.
#'
#' @inheritParams trispace
#'
#' @return A data frame of class [`colspace`] consisting of the following columns:
#' * `s`, `m`, `l`: the quantum catch data used to calculate the remaining
#' variables.
#' * `x`, `y`: coordinates for the points in coc space
#' * `r.vec`: the r vector (saturation, distance from the center using a
#' city-block metric).
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "apis", qcatch = "Ei", relative = FALSE, vonkries = TRUE)
#' coc.flowers <- colspace(vis.flowers, space = "coc")
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @keywords internal
#'
#' @references Backhaus W. (1991). Color opponent coding in the visual system
#'  of the honeybee. Vision Research, 31, 1381-1397.

coc <- function(vismodeldata) {
  if (is.vismodel(vismodeldata)) {
    if (attr(vismodeldata, "relative")) {
      stop("Quantum catches are relative, which is not required in the coc model", call. = FALSE)
    }
    if (attr(vismodeldata, "qcatch") != "Ei") { # todo: more flexible
      stop("Quantum catches are not hyperbolically transformed, as required for the coc model", call. = FALSE)
    }
    if (!isTRUE(attr(vismodeldata, "vonkries"))) {
      stop("Quantum catches are not von-Kries transformed, as required for the coc model", call. = FALSE)
    }
  } else {

  }

  dat <- check_data_for_colspace(
    vismodeldata,
    c("s", "m", "l"),
    force_relative = FALSE
  )

  if (isTRUE(all.equal(rowSums(dat), rep(1, nrow(dat)), check.attributes = FALSE))) {
    stop(
      "Quantum catches are relative, which is not required in the coc model and may produce unexpected results",
      call. = FALSE
    )
  }

  # coordinates
  x <- (-9.86 * dat$s) + (7.7 * dat$m) + (2.16 * dat$l)
  y <- (-5.17 * dat$s) + (20.25 * dat$m) - (15.08 * dat$l)

  # colorimetrics
  r.vec <- abs(x) + abs(y) # city-block from origin
  # h.theta <- atan2(y, x)

  res <- data.frame(dat, x, y, r.vec, row.names = rownames(dat))

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "coc"
  attr(res, "conenumb") <- 3
  res <- copy_attributes(
    res,
    vismodeldata,
    which = c(
      "qcatch", "visualsystem.chromatic", "visualsystem.achromatic",
      "illuminant", "background", "relative", "vonkries",
      "data.visualsystem.chromatic", "data.visualsystem.achromatic",
      "data.background"
    )
  )

  res
}
