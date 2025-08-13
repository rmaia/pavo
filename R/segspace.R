#' Segment classification
#'
#' Calculates segment classification measures as defined in Endler (1990).
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the
#'   result from [vismodel()] or independently calculated data (in the form of a
#'   data frame with columns named 'S1', 'S2', 'S3', 'S4', and, optionally,
#'   'lum', representing a generic 'tetrachromatic' viewer).
#'
#' @return A data frame of class [`colspace`] consisting of the following columns:
#' * `S1`, `S2`, `S3`, `S4`: the relative reflectance at each of the four
#' segments.
#' * `LM`, `MS`: segment scores
#' * `C`, `H`, `B`: 'chroma', 'hue' (degrees), and 'brightness' in the segment
#' classification space
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' data(sicalis)
#' vis.sic <- vismodel(sicalis, visual = "segment", achromatic = "all")
#' seg.sic <- colspace(vis.sic, space = "segment")
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}
#'
#' @references Endler, J. A. (1990) On the measurement and classification of
#' colour in studies of animal colour patterns. Biological Journal of the
#' Linnean Society, 41, 315-352.

segspace <- function(vismodeldata) {

  if (!is.null(vismodeldata$lum)) {
    B <- vismodeldata$lum
  } else {
    B <- NA
  }

  dat <- check_data_for_colspace(
    vismodeldata,
    paste0("S", 1:4),
    force_relative = TRUE
  )

  # LM/MS

  LM <- dat$S4 - dat$S2
  MS <- dat$S3 - dat$S1

  # Colormetrics
  C <- sqrt(LM^2 + MS^2)
  H <- asin(MS / C) * (180 / pi)

  res <- data.frame(
    dat, LM, MS, C, H, B,
    row.names = rownames(dat),
    stringsAsFactors = FALSE
  )

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "clrsp") <- "segment"
  attr(res, "conenumb") <- 4
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
