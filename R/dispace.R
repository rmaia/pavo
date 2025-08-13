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
  dat <- check_data_for_colspace(
    vismodeldata,
    c("s", "l"),
    force_relative = TRUE
  )

  # coordinate
  ref <- matrix(c(-1 / sqrt(2), 1 / sqrt(2)), nrow = 2, ncol = 1)

  x <- bary2cart(ref, as.matrix(dat))

  # colorimetrics?
  r.vec <- abs(x)

  res <- data.frame(dat, x, r.vec, row.names = rownames(dat))

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "dispace"
  attr(res, "conenumb") <- 2
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
  if (!is.null(maxqcatches) && ncol(maxqcatches) == 2) {
    maxqcatches <- maxqcatches / rowSums(maxqcatches)
    attr(res, "data.maxgamut") <- bary2cart(ref, maxqcatches)
  } else {
    attr(res, "data.maxgamut") <- NA
  }

  res
}
