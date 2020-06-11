#' CIE colour spaces
#'
#' Calculates coordinates and colorimetric variables that represent reflectance spectra
#' in either the CIEXYZ (1931), CIELAB (1971), or CIELCh (1971) colourspaces.
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the result
#'  from [vismodel()] or independently calculated data (in the form of a
#'  data frame with three columns representing trichromatic viewer).
#' @param space (required) Choice between XYZ (default), LAB, or LCh colour models.
#' @param visual the visual system used when estimating XYZ values, if `vismodeldata` are
#' not the result of a call to `vismodel()`. Options are:
#' - a data frame such as one produced containing by [sensmodel()], containing
#'    user-defined sensitivity data for the receptors involved in colour vision.
#'    The data frame must contain a `'wl'` column with the range of wavelengths included,
#'    and the sensitivity for each other cone as a column.
#' - `'cie2'`: 2-degree colour matching functions for CIE models of human
#'  colour vision. Functions are linear transformations of the 2-degree cone fundamentals
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' - `'cie10'`: 10-degree colour matching functions for CIE models of human
#'  colour vision. Functions are linear transformations of the 10-degree cone fundamentals
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' @param illum the illuminant used when estimating XYZ values, if `vismodeldata` are
#' not the result of a call to `vismodel()`. Either a data frame containing a `'wl'` column
#' and the illuminant spectrum, or one of the built-in options:
#' - `'D65'`: standard daylight.
#' - `'bluesky'` open blue sky.
#' - `'forestshade'` forest shade.
#'
#' @return Object of class [`colspace`] containing:
#' * `X, Y, Z`: Tristimulus values.
#' * `x, y, z`: Cartesian coordinates, when using `space = XYZ`.
#' * `L, a, b`: Lightness, `L`, and colour-opponent `a` (redness-greenness) and
#' `b` (yellowness-blueness) values, in a Cartesian coordinate space. Returned
#' when using `space = LAB`.
#' * `L, a, b, C, h`: Lightness, `L`, colour-opponent `a` (redness-greenness)
#' and `b` (yellowness-blueness) values, as well as chroma `C` and hue-angle `h`
#' (degrees), the latter of which are cylindrical representations of `a` and `b`
#' from the CIELAB model. Returned when using `space = LCh`.
#'
#' @examples
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = "cie10", illum = "D65", vonkries = TRUE, relative = FALSE)
#' flowers.ciexyz <- colspace(vis.flowers, space = "ciexyz")
#' flowers.cielab <- colspace(vis.flowers, space = "cielab")
#' flowers.cielch <- colspace(vis.flowers, space = "cielch")
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

cie <- function(vismodeldata,
                space = c("XYZ", "LAB", "LCh"),
                visual = c("cie2", "cie10"),
                illum = c("D65", "bluesky", "forestshade")) {
  space <- tryCatch(match.arg(space),
    error = function(e) {
      message("Invalid space arg. Defaulting to XYZ")
      return("XYZ")
    }
  )

  X <- vismodeldata[, names(vismodeldata) %in% c("X", "cie2_X", "cie10_X")]
  Y <- vismodeldata[, names(vismodeldata) %in% c("Y", "cie2_Y", "cie10_Y")]
  Z <- vismodeldata[, names(vismodeldata) %in% c("Z", "cie2_Z", "cie10_Z")]

  # Coordinates in the chosen CIE space
  if (space == "XYZ") {
    x <- X / (X + Y + Z)
    y <- Y / (X + Y + Z)
    z <- Z / (X + Y + Z)
  } else if (space == "LAB" | space == "LCh") {

    # Calculate tristimulus values for neutral point. First need to
    # re-grab original sensitivity and illuminant data.
    if ("vismodel" %in% class(vismodeldata)) {
      S <- attr(vismodeldata, "data.visualsystem.chromatic")
      illum <- attr(vismodeldata, "data.illuminant") # Illuminant
    } else {
      # Grab built-in data
      sens <- vissyst
      bgil <- bgandilum

      # Match user-specified arguments
      visual2 <- tryCatch(
        match.arg(visual),
        error = function(e) "user-defined"
      )
      illum2 <- tryCatch(
        match.arg(illum),
        error = function(e) "user-defined"
      )

      # Grab the relevant data
      S <- switch(visual2,
        "user-defined" = isolate_wl(visual, keep = "spec"),
        "cie2" = sens[, grep(visual2, names(sens))],
        "cie10" = sens[, grep(visual2, names(sens))]
      )
      illum <- switch(illum2,
        "user-defined" = isolate_wl(illum, keep = "spec"),
        "D65" = bgil[, grep(illum2, names(bgil))],
        "bluesky" = bgil[, grep(illum2, names(bgil))],
        "forestshade" = bgil[, grep(illum2, names(bgil))]
      )
    }

    Xn <- sum(S[, 1] * illum)
    Yn <- sum(S[, 2] * illum)
    Zn <- sum(S[, 3] * illum)

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

    # Rescale
    L <- L * 10
    a <- a * 10
    b <- b * 10

    # LCh calculator
    C <- sqrt(a^2 + b^2)
    h <- atan2(b, a) * (180 / pi)
    h[h < 0] <- h[h < 0] + 360
  }

  if (space == "XYZ") {
    res <- data.frame(X, Y, Z, x, y, z, row.names = rownames(vismodeldata))
  } else if (space == "LAB") {
    res <- data.frame(X, Y, Z, L, a, b, row.names = rownames(vismodeldata))
  } else if (space == "LCh") {
    res <- data.frame(X, Y, Z, L, a, b, C, h, row.names = rownames(vismodeldata))
  }

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
