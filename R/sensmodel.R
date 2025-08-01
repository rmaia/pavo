#' Modeling spectral sensitivity
#'
#' Models spectral sensitivity (with oil droplets; optional) based on peak cone
#' sensitivity according to the models of Govardovskii et al. (2000) and Hart &
#' Vorobyev (2005).
#'
#' @param peaksens (required) a vector with peak sensitivities for the cones to
#'   model.
#' @param range a vector of length 2 for the range over which to calculate the
#'   spectral sensitivities (defaults to 300nm to 700nm).
#' @param lambdacut a vector of same length as peaksens that lists the cut-off
#'   wavelength value for oil droplets. Needs either `Bmid` or `oiltype` to also
#'   be entered. See Hart and Vorobyev (2005).
#' @param Bmid a vector of same length as peaksens that lists the gradient of
#'   line tangent to the absorbance spectrum of the oil droplets. See Hart and
#'   Vorobyev (2005).
#' @param oiltype a list of same length as peaksens that lists the oil droplet
#'   types (currently accepts only "T", C", "Y", "R", "P") when Bmid is not
#'   known. Calculates Bmid based on the regression equations found in Hart ad
#'   Vorobyev (2005).
#' @param beta logical. If `TRUE` the sensitivities will include the beta peak
#' See Govardovskii et al.(2000) (defaults to `TRUE`).
#' @param om a vector of same length as `range1`-`range2` that contains ocular
#'   media transmission data. If included, cone sensitivity will be corrected
#'   for ocular media transmission. Currently accepts "bird" using values from
#'   Hart et al. (2005), or user-defined values.
#' @param integrate logical. If `TRUE`, each curve is transformed to have a
#'   total area under the curve of 1 (best for visual models; defaults to
#'   `TRUE`). NOTE: integration is applied before any effects of ocular media
#'   are considered, for compatibility with visual model procedures.
#' @param sensnames A vector equal in length to `peaksens`, specifying custom
#'   names for the resulting sensitivity curves (e.g. c('s', 'm', 'l') for
#'   short-, medium- and long-wavelength sensitive receptors.)
#'
#' @return A data frame of class `rspec` containing each cone model as a column.
#'
#' @export
#'
#' @examples
#' # Blue tit visual system based on Hart et al (2000)
#' bluesens <- sensmodel(c(371, 448, 502, 563),
#'   beta = FALSE,
#'   lambdacut = c(330, 413, 507, 572),
#'   oiltype = c("T", "C", "Y", "R"), om = TRUE
#' )
#'
#' # Danio aequipinnatus based on Govardovskii et al. (2000)
#' daniosens <- sensmodel(c(357, 411, 477, 569))
#'
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @references Govardovskii VI, Fyhrquist N, Reuter T, Kuzmin DG and Donner K.
#'   2000. In search of the visual pigment template. Visual Neuroscience
#'   17:509-528, \doi{10.1017/S0952523800174036}
#' @references Hart NS, and Vorobyev M. 2005. Modeling oil droplet absorption
#'   spectra and spectral sensitivities of bird cone photoreceptors. Journal of
#'   Comparative Physiology A. 191: 381-392, \doi{10.1007/s00359-004-0595-3}
#' @references Hart NS, Partridge JC, Cuthill IC, Bennett AT (2000) Visual
#'   pigments, oil droplets, ocular media and cone photoreceptor distribution in
#'   two species of passerine bird: the blue tit (*Parus caeruleus* L.) and the
#'   blackbird (*Turdus merula* L.). J Comp Physiol A 186:375-387,
#'   \doi{10.1007/s003590050437}



sensmodel <- function(peaksens, range = c(300, 700), lambdacut = NULL, Bmid = NULL,
                      oiltype = NULL, beta = TRUE, om = NULL, integrate = TRUE, sensnames = paste0("lmax", peaksens)) {
  if (!is.null(lambdacut)) {
    if (is.null(Bmid) && is.null(oiltype)) stop("Bmid or oiltype must be included when including a lambdacut vector", call. = FALSE)
    if (length(lambdacut) != length(peaksens)) stop("lambdacut must be same length as peaksens", call. = FALSE)
  }

  if (!is.null(Bmid) && is.null(lambdacut)) { # Change once oil type corrected
    stop("lambdacut has to be provided together with Bmid", call. = FALSE)
  }

  if (!is.null(lambdacut) && !is.null(Bmid) && !is.null(oiltype)) {
    stop("only 2 of lambdacut, Bmid, and oiltype can be provided", call. = FALSE)
  }
  if (!is.null(lambdacut) && !is.null(oiltype) && length(lambdacut) != length(oiltype)) {
    stop("lambdacut and oiltype must be of same length", call. = FALSE)
  }
  if (!is.null(lambdacut) && !is.null(Bmid) && length(lambdacut) != length(Bmid)) {
    stop("lambdacut and Bmid must be of same length", call. = FALSE)
  }

  wl <- range[1]:range[2]

  sensecurves <- matrix(wl, ncol = length(wl), nrow = length(peaksens), byrow = TRUE)

  peaks <- 1 / (exp(69.7 * (0.8795 + 0.0459 * exp(-(peaksens - range[1])^2 / 11940) - (peaksens / sensecurves)))
       + exp(28 * (0.922 - peaksens / sensecurves)) + exp(-14.9 * (1.104 - (peaksens / sensecurves))) + 0.674)

  if (beta) {
    betabands <- 0.26 * exp(-((sensecurves - (189 + 0.315 * peaksens)) / (-40.5 + 0.195 * peaksens))^2)
    peaks <- peaks + betabands
  }

  peaks <- peaks / apply(peaks, 1, max)

  if (!is.null(lambdacut) && !is.null(Bmid)) {
    T.oil <- exp(-exp(-2.89 * Bmid * (sensecurves - lambdacut) + 1.08))
    if (!identical(is.na(lambdacut), is.na(Bmid))) {
      warning("NA in lambdacut not paired with NA in Bmid, value of Bmid omitted", call. = FALSE)
      T.oil[is.na(lambdacut)] <- 1
    }
    peaks <- peaks * T.oil
  }

  for (i in seq_along(peaksens)) {
    if (!is.null(lambdacut) && !is.null(oiltype)) {
      if (oiltype[i] == "T") {
        T.oil <- 1
      } else {
        if (oiltype[i] == "C") oil <- c(0.99, 24.38)
        if (oiltype[i] == "Y") oil <- c(0.9, 70.03)
        if (oiltype[i] == "R") oil <- c(0.99, 28.65)
        if (oiltype[i] == "P") oil <- c(0.96, 33.57)

        # Oil droplet transmission from Hart and Vorobyev (2005)
        T.oil <- exp(-exp(-2.89 * (0.5 / ((oil[1] * lambdacut[i] + oil[2]) - lambdacut[i])) *
          (sensecurves[i, ] - lambdacut[i]) + 1.08))
      }
      peaks[i, ] <- peaks[i, ] * T.oil
    }
  }

  # Apply integration
  if (integrate) {
    peaks <- peaks / rowSums(peaks)
  }

  # Apply ocular media transmission correction
  if (!is.null(om)) {
    if (identical(om, "bird")) {
      T.e <- log(8.928 * 10^-13 * sensecurves^5 - 2.595 * 10^-9 *
                   sensecurves^4 + 3.006 * 10^-6 *
                   sensecurves^3 - 0.001736 * sensecurves^2 + 0.5013 *
                   sensecurves - 55.56)
      T.e[which(T.e < 0)] <- 0
    } else {
      T.e <- om
    }
    peaks <- peaks * T.e
  }

  sensecurves <- as.data.frame(t(peaks))

  if (length(sensnames) != length(sensecurves)) {
    message("The length of argument 'sensnames' does not equal the number of curves specified by 'peaksens'. Reverting to default names.")
    sensnames <- paste0("lmax", peaksens)
  }

  names(sensecurves) <- sensnames
  sensecurves <- cbind(wl, sensecurves)

  class(sensecurves) <- c("sensmod", "rspec", "data.frame")

  if (is.null(om)) {
    attr(sensecurves, "om") <- FALSE
  } else {
    attr(sensecurves, "om") <- TRUE
  }

  sensecurves
}
