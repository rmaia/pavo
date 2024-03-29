#' Trichromatic colour space
#'
#' Calculates coordinates and colourimetric variables that represent reflectance spectra
#' in a trichromatic chromaticity space.
#'
#' @param vismodeldata (required) quantum catch colour data. Can be either the result
#'  from [vismodel()] or independently calculated data (in the form of a data frame
#'  with three columns named 's', 'm', 'l', representing a trichromatic viewer).
#'
#' @return A data frame of class [`colspace`] consisting of the following columns:
#' * `s`, `m`, `l`: the quantum catch data used to calculate
#'  the remaining variables.
#' * `x`, `y`: cartesian coordinates for the points in the
#'  Maxwell triangle.
#' * `h.theta`: angle theta, in radians, determining the hue of the colour.
#' * `r.vec`: the r vector (saturation, distance from the center).
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "apis", achromatic = "l")
#' tri.flowers <- colspace(vis.flowers, space = "tri")
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @importFrom geometry bary2cart
#'
#' @keywords internal
#'
#' @references Maxwell JC. (1970). On color vision. In: Macadam, D. L. (ed)
#'  Sources of Color Science. Cambridge, MIT Press, 1872 - 1873.
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision -
#'  behavioural tests and physiological concepts. Biological Reviews, 78,
#'  81 - 118.
#' @references MacLeod DIA, Boynton RM. (1979). Chromaticity diagram showing
#'  cone excitation by stimuli of equal luminance. Journal of the Optical
#'  Society of America, 69, 1183-1186.

trispace <- function(vismodeldata) {
  dat <- vismodeldata

  # if object is vismodel:
  if (is.vismodel(dat)) {
    # check if trichromat
    if (attr(dat, "conenumb") < 3) {
      stop("vismodel input is not trichromatic", call. = FALSE)
    }

    if (attr(dat, "conenumb") > 3) {
      warning("vismodel input is not trichromatic, considering first three receptors only", call. = FALSE)
      attr(vismodeldata, "data.maxqcatches") <- attr(vismodeldata, "data.maxqcatches")[, seq_len(3)]
    }

    # check if relative
    if (!attr(dat, "relative")) {
      dat <- dat[, seq_len(3)]
      dat <- dat / rowSums(dat)
      class(dat) <- class(vismodeldata)
      warning("Quantum catch are not relative, and have been transformed", call. = FALSE)
      attr(vismodeldata, "relative") <- TRUE
    }
  } else { # if not, check if it has more (or less) than 3 columns
    if (ncol(dat) < 3) {
      stop("Input data is not a ", dQuote("vismodel"), " object and has fewer than three columns", call. = FALSE)
    }
    if (ncol(dat) == 3) {
      warning("Input data is not a ", dQuote("vismodel"),
        " object; treating columns as standardized quantum catch for ",
        dQuote("s"), ", ", dQuote("m"), ", and ", dQuote("l"), " receptors, respectively",
        call. = FALSE
      )
    }

    if (ncol(dat) > 3) {
      warning("Input data is not a ", dQuote("vismodel"),
        " object *and* has more than three columns; treating the first three columns as standardized quantum catch for ",
        dQuote("s"), ", ", dQuote("m"), ", and ", dQuote("l"), " receptors, respectively",
        call. = FALSE
      )
      attr(vismodeldata, "data.maxqcatches") <- attr(vismodeldata, "data.maxqcatches")[, seq_len(3)]
    }

    dat <- dat[, seq_len(3)]
    names(dat) <- c("s", "m", "l")

    # Check that all rows sum to 1 (taking into account R floating point issue)
    if (!isTRUE(all.equal(rowSums(dat), rep(1, nrow(dat)), check.attributes = FALSE))) {
      dat <- dat / rowSums(dat)
      warning("Quantum catch are not relative, and have been transformed.", call. = FALSE)
      attr(vismodeldata, "relative") <- TRUE
    }
  }

  if (all(c("s", "m", "l") %in% names(dat))) {
    s <- dat[, "s"]
    m <- dat[, "m"]
    l <- dat[, "l"]
  } else if (all(c("X", "Y", "Z") %in% names(dat))) {
    s <- dat[, "Z"]
    m <- dat[, "Y"]
    l <- dat[, "X"]
  } else {
    warning("Could not find columns named ", dQuote("s"), ", ", dQuote("m"), ", and ", dQuote("l"), ", using first three columns instead.", call. = FALSE)
    s <- dat[, 1]
    m <- dat[, 2]
    l <- dat[, 3]
  }

  # cartesian coordinates
  ref <- matrix(c(
    0, sqrt(2 / 3),
    -1 / sqrt(2), -1 / sqrt(6),
    1 / sqrt(2), -1 / sqrt(6)
  ), nrow = 3, ncol = 2, byrow = TRUE)

  coords <- bary2cart(ref, cbind(s, m, l))

  x <- coords[, 1]
  y <- coords[, 2]

  # colorimetrics
  r.vec <- sqrt(x^2 + y^2)
  h.theta <- atan2(y, x)

  res <- data.frame(s, m, l, x, y, h.theta, r.vec, row.names = rownames(dat))

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "trispace"
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

  maxqcatches <- attr(vismodeldata, "data.maxqcatches")
  if (!is.null(maxqcatches) && ncol(maxqcatches) == 3) {
    maxqcatches <- maxqcatches / rowSums(maxqcatches)
    attr(res, "data.maxgamut") <- bary2cart(ref, maxqcatches)
  } else {
    attr(res, "data.maxgamut") <- NA
  }

  res
}
