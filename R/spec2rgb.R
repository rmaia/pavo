#' Spectrum to rgb colour conversion
#'
#' Calculates rgb values from spectra based on human colour matching functions.
#'
#' @inheritParams vismodel
#' @param alpha alpha value to use for colours (defaults to 1, opaque).
#'
#' @return A character vector consisting of hexadecimal colour values
#' for passing to further plotting functions.
#'
#' @export
#'
#' @examples
#' data(teal)
#' spec2rgb(teal)
#'
#' # Plot data using estimated perceived colour
#' plot(teal, col = spec2rgb(teal), type = "overlay")
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @references CIE(1932). Commission Internationale de l'Eclairage Proceedings, 1931. Cambridge: Cambridge University Press.
#'
#' @importFrom grDevices convertColor rgb

spec2rgb <- function(rspecdata, alpha = 1) {
  XYZ <- vismodel(rspecdata,
    visual = "cie10", illum = "D65",
    vonkries = TRUE, relative = FALSE
  )

  XYZ <- XYZ[, !(names(XYZ) %in% "lum")]

  rgb1 <- convertColor(XYZ, from = "XYZ", to = "sRGB")

  rgb(rgb1, alpha = alpha, names = rownames(XYZ))
}
