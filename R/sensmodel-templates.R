########################
# VISUAL PIGMENT TEMPLATES #
########################

# Internal template functions used by sensmodel().
#
# Calling contract, which any template added here must follow:
#
#   f(peaksens, wlmat, beta)
#
#   peaksens  numeric vector of peak sensitivities, length n.
#   wlmat     n x m matrix of wavelengths in nm, one row per pigment, each row
#             identical. Arithmetic between `peaksens` and `wlmat` recycles
#             column-major and therefore aligns per-row, so templates can be
#             written in vectorised form without looping over pigments.
#   beta      logical, whether to include the beta band. Beta band expressions
#             differ between templates and some templates have none, so each
#             template owns its own handling rather than sensmodel() adding a
#             common one.
#
#   Returns an n x m matrix of sensitivities on a linear (not log) scale. The
#   return value need not be normalised: sensmodel() peak-normalises every
#   template to a maximum of 1 before applying oil droplets, ocular media and
#   integration, all of which are template-agnostic.


# Govardovskii et al. (2000), vitamin A1 chromophore.
#
# The alpha band is equation 2 of Stavenga (2010), which restates Govardovskii
# et al. (2000). The 300 in the `a` term is a constant of that expression, not a
# reference to the `range` argument of sensmodel(); coding it as `range[1]`
# applies the short-wavelength narrowing correction according to the requested
# range rather than the pigment's peak sensitivity, which was the bug fixed in
# pavo 2.10.0. The beta band is Govardovskii's Gaussian, whose peak wavelength
# and width both scale with peaksens.
sens_govardovskii <- function(peaksens, wlmat, beta = TRUE) {
  x <- peaksens / wlmat

  a <- 0.8795 + 0.0459 * exp(-(peaksens - 300)^2 / 11940)

  peaks <- 1 / (exp(69.7 * (a - x)) +
    exp(28 * (0.922 - x)) +
    exp(-14.9 * (1.104 - x)) +
    0.674)

  if (beta) {
    betabands <- 0.26 * exp(-((wlmat - (189 + 0.315 * peaksens)) /
      (-40.5 + 0.195 * peaksens))^2)
    peaks <- peaks + betabands
  }

  peaks
}
