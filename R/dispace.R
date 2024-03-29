#' Dichromatic colour space
#'
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in a dichromatic colour space.
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from [vismodel()] or independently calculated data (in the form of a data frame
#'  with two columns named 's' and 'l', representing a dichromatic viewer's receptors).
#'
#' @return A data frame of class [`colspace`] consisting of the following columns:
#' * `s`, `l`: the quantum catch data used to calculate
#'  the remaining variables.
#' * `x`: the coordinate of the stimulus along a segment
#' * `r.vec`: the r vector (saturation, distance from the center).
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "canis")
#' di.flowers <- colspace(vis.flowers, space = "di")
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @keywords internal
#'
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision -
#'  behavioural tests and physiological concepts. Biological Reviews, 78,
#'  81 - 118.

dispace <- function(vismodeldata) {
  dat <- vismodeldata

  # if object is vismodel:
  if (is.vismodel(dat)) {
    # check if trichromat
    if (attr(dat, "conenumb") < 2) {
      stop("vismodel input is not dichromatic", call. = FALSE)
    }

    if (attr(dat, "conenumb") > 2) {
      warning("vismodel input is not dichromatic, considering first two receptors only", call. = FALSE)
      attr(vismodeldata, "data.maxqcatches") <- attr(vismodeldata, "data.maxqcatches")[, seq_len(2)]
    }

    # check if relative
    if (!attr(dat, "relative")) {
      dat <- dat[, 1:2]
      dat <- dat / rowSums(dat)
      class(dat) <- class(vismodeldata)
      warning("Quantum catch are not relative, and have been transformed", call. = FALSE)
      attr(vismodeldata, "relative") <- TRUE
    }
  } else { # if not, check if it has more (or less) than 2 columns
    if (ncol(dat) < 2) {
      stop("Input data is not a ", dQuote("vismodel"), " object and has fewer than two columns", call. = FALSE)
    }
    if (ncol(dat) == 2) {
      warning("Input data is not a ", dQuote("vismodel"),
        " object; treating columns as standardized quantum catch for ",
        dQuote("s"), " and ", dQuote("l"), " receptors, respectively",
        call. = FALSE
      )
    }

    if (ncol(dat) > 2) {
      warning("Input data is not a ", dQuote("vismodel"),
        " object *and* has more than two columns; treating the first two columns as standardized quantum catch for ",
        dQuote("s"), ", and ", dQuote("l"), " receptors, respectively",
        call. = FALSE
      )
      attr(vismodeldata, "data.maxqcatches") <- attr(vismodeldata, "data.maxqcatches")[, seq_len(2)]
    }

    dat <- dat[, 1:2]
    names(dat) <- c("s", "l")

    # Check that all rows sum to 1 (taking into account R floating point issue)
    if (!isTRUE(all.equal(rowSums(dat), rep(1, nrow(dat)), check.attributes = FALSE))) {
      dat <- dat / rowSums(dat)
      warning("Quantum catch are not relative, and have been transformed", call. = FALSE)
      attr(vismodeldata, "relative") <- TRUE
    }
  }

  if (all(c("s", "l") %in% names(dat))) {
    s <- dat[, "s"]
    l <- dat[, "l"]
  } else {
    warning("Could not find columns named ", dQuote("s"), ", and ",
      dQuote("l"), ", using first two columns instead.",
      call. = FALSE
    )
    s <- dat[, 1]
    l <- dat[, 2]
  }

  # coordinate
  ref <- matrix(c(-1 / sqrt(2), 1 / sqrt(2)), nrow = 2, ncol = 1)

  x <- bary2cart(ref, cbind(s, l))

  # colorimetrics?
  r.vec <- abs(x)

  res <- data.frame(s, l, x, r.vec, row.names = rownames(dat))

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "dispace"
  attr(res, "conenumb") <- 2
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
  if (!is.null(maxqcatches) && ncol(maxqcatches) == 2) {
    maxqcatches <- maxqcatches / rowSums(maxqcatches)
    attr(res, "data.maxgamut") <- bary2cart(ref, maxqcatches)
  } else {
    attr(res, "data.maxgamut") <- NA
  }

  res
}
