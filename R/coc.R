#' Color opponent coding model
#'
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in the color opponent coding model of hymenopteran vision.
#'
#' @inheritParams trispace
#'
#' @return A data frame of class \code{colspace} consisting of the following columns:
#' @return \code{s}, \code{m}, \code{l}: the quantum catch data used to calculate
#'  the remaining variables.
#' @return \code{x}, \code{y}: coordinates for the points in coc space
#' @return \code{r.vec}: the r vector (saturation, distance from the center using
#'  a city-block metric).
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
  dat <- vismodeldata

  # if object is vismodel:
  if (is.vismodel(dat)) {

    # check if trichromat
    if (attr(dat, "conenumb") < 3) {
      stop("vismodel input is not trichromatic", call. = FALSE)
    }

    if (attr(dat, "conenumb") > 3) {
      warning("vismodel input is not trichromatic, considering first three receptors only", call. = FALSE)
    }

    if (attr(dat, "relative")) {
      stop("Quantum catches are relative, which is not required in the coc model", call. = FALSE)
    }

    if (attr(dat, "qcatch") != "Ei") { # todo: more flexible
      stop("Quantum catches are not hyperbolically transformed, as required for the coc model", call. = FALSE)
    }

    if (!isTRUE(attr(dat, "vonkries"))) {
      stop("Quantum catches are not von-Kries transformed, as required for the coc model", call. = FALSE)
    }
  }

  # if not, check if it has more (or less) than 3 columns
  else {
    if (ncol(dat) < 3) {
      stop("Input data is not a ", dQuote("vismodel"),
        " object and has fewer than three columns",
        call. = FALSE
      )
    }
    if (ncol(dat) == 3) {
      warning("Input data is not a ", dQuote("vismodel"),
        " object; treating columns as quantum catch for ",
        dQuote("s"), ", ", dQuote("m"), ", and ", dQuote("l"),
        " receptors, respectively",
        call. = FALSE
      )
    }

    if (ncol(dat) > 3) {
      warning("Input data is not a ", dQuote("vismodel"),
        " object *and* has more than three columns; treating the first three columns as quantum catch for ",
        dQuote("s"), ", ", dQuote("m"), ", and ", dQuote("l"),
        " receptors, respectively",
        call. = FALSE
      )
    }

    dat <- dat[, 1:3]
    names(dat) <- c("s", "m", "l")

    if (isTRUE(all.equal(rowSums(dat), rep(1, nrow(dat)), check.attributes = FALSE))) {
      stop("Quantum catches are relative, which is not required in the coc model and may produce unexpected results", call. = FALSE)
    }
  }

  if (all(c("s", "m", "l") %in% names(dat))) {
    s <- dat[, "s"]
    m <- dat[, "m"]
    l <- dat[, "l"]
  } else {
    warning("Could not find columns named ", dQuote("s"), ", ", dQuote("m"), ", and ",
      dQuote("l"), ", using first three columns instead.",
      call. = FALSE
    )
    s <- dat[, 1]
    m <- dat[, 2]
    l <- dat[, 3]
  }

  # coordinates
  x <- (-9.86 * s) + (7.7 * m) + (2.16 * l)
  y <- (-5.17 * s) + (20.25 * m) - (15.08 * l)

  # colorimetrics
  r.vec <- abs(x) + abs(y) # city-block from origin
  # h.theta <- atan2(y, x)

  res.p <- data.frame(s, m, l, x, y, r.vec, row.names = rownames(dat))

  res <- res.p

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "coc"
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
