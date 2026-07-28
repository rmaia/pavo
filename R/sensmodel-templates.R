############################
# VISUAL PIGMENT TEMPLATES #
############################

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
#
# Chromophore is part of the template name rather than a separate argument,
# because support for it is ragged: templates not yet implemented here are
# A1-only (Lamb 1995, Baylor et al. 1987), while SSH also covers A4.


sens_template <- function(template, peaksens, wlmat, beta) {
  switch(template,
    govardovskii_a1 = sens_govardovskii(peaksens, wlmat, beta, chromophore = "A1"),
    govardovskii_a2 = sens_govardovskii(peaksens, wlmat, beta, chromophore = "A2"),
    ssh_a1 = sens_ssh(peaksens, wlmat, beta, chromophore = "A1"),
    ssh_a2 = sens_ssh(peaksens, wlmat, beta, chromophore = "A2"),
    stop("unknown template '", template, "'", call. = FALSE)
  )
}


# Govardovskii et al. (2000).
#
# The alpha band is their equation 1, a form due to Lamb (1995), refitted. Under
# the Mansfield-MacNichol transform the parameters governing the long-wave slope
# would be constant; they are not, so Govardovskii et al. make them functions of
# peaksens. For A1 that is `a` alone, their equation 2; for A2 it is both `A` and
# `a`, their equations 6a and 6b, and the remaining five constants differ too.
#
# The 300 in the A1 `a` term is a constant of that expression, not a reference to
# the `range` argument of sensmodel(); coding it as `range[1]` applies the
# short-wavelength narrowing correction according to the requested range rather
# than the pigment's peak sensitivity, which was the bug fixed in pavo 2.10.0.
#
# The beta band is their equation 4, a Gaussian whose peak wavelength and width
# are both linear in peaksens for A1 (equations 5a, 5b); for A2 the peak is
# linear but the width is quadratic (equations 8a, 8b), and the amplitude is
# higher. Because both scale with peaksens, neither chromophore suffers the peak
# displacement that the fixed-wavelength SSH beta band produces.
#
# Fitted over lmax 357-620 nm. The A2 sample spans roughly 440-620 nm.
sens_govardovskii <- function(peaksens, wlmat, beta = TRUE, chromophore = "A1") {
  x <- peaksens / wlmat

  if (identical(chromophore, "A1")) {
    A <- 69.7
    a <- 0.8795 + 0.0459 * exp(-(peaksens - 300)^2 / 11940)
    B <- 28
    b <- 0.922
    Cc <- -14.9
    cc <- 1.104
    D <- 0.674

    beta_amp <- 0.26
    beta_peak <- 189 + 0.315 * peaksens
    beta_width <- -40.5 + 0.195 * peaksens
  } else {
    A <- 62.7 + 1.834 * exp((peaksens - 625) / 54.2)
    a <- 0.875 + 0.0268 * exp((peaksens - 665) / 40.7)
    B <- 20.85
    b <- 0.9101
    Cc <- -10.37
    cc <- 1.1123
    D <- 0.5343

    beta_amp <- 0.37
    beta_peak <- 216.7 + 0.287 * peaksens
    beta_width <- 317 - 1.149 * peaksens + 0.00124 * peaksens^2
  }

  peaks <- 1 / (exp(A * (a - x)) +
    exp(B * (b - x)) +
    exp(Cc * (cc - x)) +
    D)

  if (beta) {
    betabands <- beta_amp * exp(-((wlmat - beta_peak) / beta_width)^2)
    peaks <- peaks + betabands
  }

  peaks
}


# Stavenga, Smits & Hoenders (1993), "SSH".
#
# Band shape is their equation 2, a modified lognormal in log10(lambda/lambdamax)
# with the restriction a2 = 3 * a1^2 / 8, which leaves the log absorbance of each
# band with a single inflection point. Coefficients are their Table 1.
#
# Two things differ from Govardovskii and matter to callers:
#
# 1. The beta band has a *fixed* peak wavelength rather than one that scales with
#    peaksens. Stavenga et al. use 350 nm for A1 in their predictions (their
#    Fig. 4), restated in Stavenga (2010); the 340 nm in Table 1 is the value
#    fitted to bovine rhodopsin specifically, not the value used for prediction.
#    For A2 no prediction value is published, so the Table 1 fit to carp
#    porphyropsin (368 nm) is used. A fixed beta band distorts the summed curve
#    for short-wavelength pigments, so sensmodel() checks peak recovery and warns.
#
# 2. Table 1 also lists a gamma band for A1 and A4. It is excluded here: it peaks
#    near 276 nm, outside the default range of sensmodel(), and Govardovskii has
#    no gamma term, so including it would make the templates non-comparable.
#
# A4 coefficients are stored but not reachable from sensmodel(), pending a use
# case. Note this is A4 (Table 1 of the paper), not the more widely encountered
# A3 chromophore of many insects.
ssh_bands <- list(
  A1 = list(
    alpha = c(A = 1, a0 = 380, a1 = 6.09),
    beta = c(A = 0.29, a0 = 247, a1 = 3.59),
    beta_peak = 350
  ),
  A2 = list(
    alpha = c(A = 1, a0 = 263, a1 = 4.45),
    beta = c(A = 0.50, a0 = 176, a1 = 1.52),
    beta_peak = 368
  ),
  A4 = list(
    alpha = c(A = 1, a0 = 420, a1 = 7.73),
    beta = c(A = 0.23, a0 = 252, a1 = 2.97),
    beta_peak = 329
  )
)

ssh_band <- function(peak, wlmat, par) {
  par <- unname(par)
  x <- log10(wlmat / peak)
  par[1] * exp(-par[2] * x^2 * (1 + par[3] * x + (3 / 8) * (par[3] * x)^2))
}

sens_ssh <- function(peaksens, wlmat, beta = TRUE, chromophore = "A1") {
  pars <- ssh_bands[[chromophore]]

  peaks <- ssh_band(peaksens, wlmat, pars$alpha)

  if (beta) {
    peaks <- peaks + ssh_band(pars$beta_peak, wlmat, pars$beta)
  }

  peaks
}
