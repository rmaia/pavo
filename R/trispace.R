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
  dat <- check_data_for_colspace(
    vismodeldata,
    c("s", "m", "l"),
    force_relative = TRUE
  )

  res <- nspace(dat)

  attr(res, "clrsp") <- "trispace"
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

  maxqcatches <- attr(vismodeldata, "data.maxqcatches")
  if (!is.null(maxqcatches) && ncol(maxqcatches) == 3) {
    maxqcatches <- maxqcatches / rowSums(maxqcatches)
    attr(res, "data.maxgamut") <- bary2cart(simplex(3), maxqcatches)
  } else {
    attr(res, "data.maxgamut") <- NA
  }

  res
}
