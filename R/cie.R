#' CIE colour spaces
#'
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in either the CIEXYZ (1931), CIELAB (1971), or CIELCH (1971) colourspaces.
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from \code{\link{vismodel}} or independently calculated data (in the form of a
#'  data frame with three columns representing trichromatic viewer).
#' @param space (required) Choice between XYZ (1931), LAB (1971), or LCH colour models.
#'
#' @return Object of class \code{colspace} containing:
#'    \itemize{
#'      \item \code{X, Y, Z}: Tristimulus values.
#'      \item \code{x, y, z}: Cartesian coordinates, when using \code{space = XYZ}.
#'      \item \code{L, a, b}: Lightness, \code{L}, and colour-opponent \code{a}
#'          (redness-greenness) and \code{b} (yellowness-blueness) values, in a
#'          Cartesian coordinate space. Returned when using \code{space = LAB}.
#'      \item \code{L, a, b, C, h}: Lightness, \code{L}, colour-opponent \code{a}
#'          (redness-greenness) and \code{b} (yellowness-blueness) values, as well as
#'          chroma \code{C} and hue-angle \code{h} (degrees), the latter of which are cylindrical
#'          representations of \code{a} and \code{b} from the CIELAB model. Returned
#'          when using \code{space = LCh}.
#'    }
#'
#' @examples
#' \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'cie10', illum = 'D65', vonkries = TRUE, relative = FALSE)
#' flowers.ciexyz <- colspace(vis.flowers, space = 'ciexyz')
#' flowers.cielab <- colspace(vis.flowers, space = 'cielab')
#' flowers.cielch <- colspace(vis.flowers, space = 'cielch')
#' }
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @keywords internal
#'
#' @references Smith T, Guild J. (1932) The CIE colorimetric standards and their use.
#'    Transactions of the Optical Society, 33(3), 73-134.
#' @references Westland S, Ripamonti C, Cheung V. (2012). Computational colour science
#'    using MATLAB. John Wiley & Sons.
#' @references Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of
#'  the middle- and long-wavelength sensitive cones derived from measurements in
#'  observers of known genotype. Vision Research, 40, 1711-1737.
#' @references CIE (2006). Fundamental chromaticity diagram with physiological axes.
#'  Parts 1 and 2. Technical Report 170-1. Vienna: Central Bureau of the Commission
#'  Internationale de l Eclairage.

cie <- function(vismodeldata, space = c("XYZ", "LAB", "LCh")) {
  space2 <- try(match.arg(space), silent = T)
  if (inherits(space2, "try-error")) {
    space <- "XYZ"
  }

  dat <- vismodeldata

  X <- dat$X
  Y <- dat$Y
  Z <- dat$Z

  # Coordinates in the chosen CIE space
  if (space == "XYZ") {
    x <- X / (X + Y + Z)
    y <- Y / (X + Y + Z)
    z <- Z / (X + Y + Z)
  } else if (space == "LAB" | space == "LCh") {

    # Calculate tristimulus values for neutral point. First need to
    # re-grab original sensitivity and illuminant data.
    S <- attr(dat, "data.visualsystem.chromatic")
    illum <- attr(dat, "data.illuminant") # Illuminant
    Xn <- sum(rep(1, 401) * S[, 1] * illum)
    Yn <- sum(rep(1, 401) * S[, 2] * illum)
    Zn <- sum(rep(1, 401) * S[, 3] * illum)
    # Xn = 94.811
    # Yn = 100
    # Zn = 107.304

    # LAB calculator
    f <- function(x) {
      if (isTRUE(x > (6 / 29)^3)) {
        x^(1 / 3)
      } else {
        (841 / 108) * x + (4 / 29)
      }
    }
    if (isTRUE(Y / Yn > 0.008856)) {
      L <- 116 * f(Y / Yn) - 16
    } else {
      L <- 903.3 * (Y / Yn)
    }
    a <- 500 * (f(X / Xn) - f(Y / Yn))
    b <- 200 * (f(Y / Yn) - f(Z / Zn))

    # LCh calculator
    C <- sqrt(a^2 + b^2)
    h <- atan2(b, a) * (180 / pi)
    h[h < 0] <- h[h < 0] + 360
  }

  if (space == "XYZ") {
    res.p <- data.frame(X, Y, Z, x, y, z, row.names = rownames(dat))
  } else if (space == "LAB") {
    res.p <- data.frame(X, Y, Z, L, a, b, row.names = rownames(dat))
  } else if (space == "LCh") {
    res.p <- data.frame(X, Y, Z, L, a, b, C, h, row.names = rownames(dat))
  }

  res <- res.p

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- paste0("CIE", space)
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
