#' Simulate spectra
#'
#' @param shape Simulate either a `"sigmoid"` ("yellow", "orange", "red", and
#' "white" colours) or a `"bell"`-shaped curve ("green", "blue", and "violet"
#' colours)
#' @param hue,brightness,saturation Colourimetric variable of the desired output
#' @param baseline Constant background reflectance for all wavelengths
#' @param lim A vector with the range of the wavelengths on which to evaluate
#' the function. Defaults to `c(300, 700)`
#'
#' @return A `rspec` object
#'
#' @details Spectra are simulated with the following functions:
#'
#' * `"sigmoid"`:
#' \deqn{R(\text{wl})=\text{baseline}+\text{brightness} \times\exp \left[-\dfrac{(\text{wl}-\text{hue})^2}{2\text{saturation}^{2}}\right]}{R(wl) = baseline + brightness * exp(-(wl-hue)^2/(2*saturation^2))}
#'
#' * `"bell"`:
#' \deqn{R(\text{wl})=\text{baseline}+\text{brightness}\left[\frac{\pi}{2}+\arctan \left(\frac{\text{wl}-\text{hue}}{\text{saturation}}\right)\right]}{R(wl) = baseline + brightness * (pi/2 + arctan((wl-hue)/sigma))}
#'
#' @examples
#' plot(simulate_spec("sigmoid", 450, 35, 10, 5))
#' plot(simulate_spec("bell", 450, 35, 10, 5))
#'
#' @export
#'
#' @references Gomez D., & Théry M. (2007), Simultaneous Crypsis and
#'  Conspicuousness in Color Patterns: Comparative Analysis of a Neotropical
#'  Rainforest Bird Community. The American Naturalist 169(S1), S42-S61.
#'  \doi{10.1086/510138}

simulate_spec <- function(shape = c("sigmoid", "bell"),
                          hue, brightness, saturation, baseline,
                          lim = c(300, 700)) {

  shape <- match.arg(shape)

  wl <- seq(lim[1], lim[2])

  if (shape == "sigmoid") {
    r <- baseline + brightness * exp(-(wl-hue)^2/(2*saturation^2))
  } else {
    r <- baseline + brightness * (pi/2 + atan2(saturation, wl-hue))
  }

  res <- data.frame(cbind(wl, r))
  class(res) <- c("rspec", "data.frame")

  return(res)
}
