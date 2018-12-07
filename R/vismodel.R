#' Visual models
#'
#' Calculates quantum catches at each photoreceptor. Both raw and relative values
#' can be returned, for use in a suite of colorspace and non-colorspace models.
#'
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec}
#'  that has wavelength range in the first column, named 'wl', and spectral measurements in the
#'  remaining columns.
#' @param qcatch Which quantal catch metric to return. Options are:
#' \itemize{
#' \item \code{'Qi'}: Quantum catch for each photoreceptor
#' \item \code{'fi'}: Quantum catch according to Fechner law (the signal of the receptor
#'  channel is proportional to the logarithm of the quantum catch)
#' \item \code{'Ei'}: Hyperbolic-transformed quantum catch, where Ei = Qi / (Qi + 1).
#' }
#' @param visual the visual system to be used. Options are:
#' \itemize{
#' 	\item a data frame such as one produced containing by \code{sensmodel}, containing
#'    user-defined sensitivity data for the receptors involved in colour vision.
#'    The data frame must contain a \code{'wl'} column with the range of wavelengths included,
#'    and the sensitivity for each other cone as a column.
#' \item \code{'apis'}: Honeybee \emph{Apis mellifera}.
#' \item \code{'avg.uv'}: average avian UV system.
#' \item \code{'avg.v'}: average avian V system.
#' \item \code{'bluetit'}: Blue tit \emph{Cyanistes caeruleus}.
#' \item \code{'canis'}: Canid \emph{Canis familiaris}.
#' \item \code{'cie2'}: 2-degree colour matching functions for CIE models of human
#'  colour vision. Functions are linear transformations of the 2-degree cone fundamentals
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' \item \code{'cie10'}: 10-degree colour matching functions for CIE models of human
#'  colour vision. Functions are linear transformations of the 10-degree cone fundamentals
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' \item \code{'ctenophorus'}: Ornate dragon lizard \emph{Ctenophorus ornatus}.
#' \item \code{'musca'}: Housefly \emph{Musca domestica}.
#' \item \code{'pfowl'}: Peafowl \emph{Pavo cristatus}.
#' \item \code{'segment'}: Generic tetrachromat 'viewer' for use in the segment analysis of Endler (1990).
#' \item \code{'star'}: Starling \emph{Sturnus vulgaris}.
#' \item \code{'habronattus'}: Jumping spider \emph{Habronattus pyrrithrix}.
#' \item \code{'rhinecanthus'}: Triggerfish \emph{Rhinecanthus aculeatus}.
#' }
#' @param achromatic the sensitivity data to be used to calculate luminance (achromatic)
#'  receptor stimulation. Either a vector containing the sensitivity for a single receptor,
#'  or one of the options:
#' \itemize{
#'  \item \code{'none'}: no achromatic stimulation calculated.
#' 	\item \code{'bt.dc'}: Blue tit \emph{Cyanistes caeruleus} double cone.
#'  \item \code{'ch.dc'}: Chicken \emph{Gallus gallus} double cone.
#'  \item \code{'st.dc'}: Starling \emph{Sturnus vulgaris} double cone.
#'  \item \code{'md.r1'}: Housefly \emph{Musca domestica} R1-6 photoreceptor.
#'  \item \code{'ra.dc'}: Triggerfish \emph{Rhinecanthus aculeatus} double cone.
#'  \item \code{'ml'}: the summed response of the two longest-wavelength photoreceptors.
#'  \item \code{'l'}: the longest-wavelength photoreceptor.
#'  \item \code{'all'}: the summed response of all photoreceptors.
#' }
#' @param illum either a vector containing the illuminant, or one of the options:
#' \itemize{
#' \item \code{'ideal'}: homogeneous illuminance of 1 across wavelengths (default)
#' \item \code{'bluesky'} open blue sky.
#' \item \code{'D65'}: standard daylight.
#' \item \code{'forestshade'} forest shade.
#' }
#' @param bkg background spectrum. Note that this will have no effect when \code{vonkries = FALSE}.
#' Either a vector containing the spectral data, or one of the options:
#' \itemize{
#' \item \code{'ideal'}: homogeneous illuminance of 1 across all wavelengths (default).
#' \item \code{'green'}: green foliage.
#' }
#' @param trans either a vector containing the ocular or environmental transmission
#' spectra, or one of the options:
#' \itemize{
#' \item \code{'ideal'}: homogeneous transmission of 1 across all wavelengths (default)
#' \item \code{'bluetit'}: blue tit \emph{Cyanistes caeruleus}
#' ocular transmission (from Hart et al. 2000).
#' \item \code{'blackbird'}: blackbird \emph{Turdus merula}
#' ocular transmission (from Hart et al. 2000).
#' }
#' @param relative should relative quantum catches be returned (i.e. is it a color
#'  space model? Defaults to \code{TRUE}).
#' @param vonkries logical. Should the von Kries color correction transformation be applied?
#'  (defaults to \code{FALSE}).
#' @param scale a value by which the illuminant will be multiplied. Useful for when the
#'  illuminant is a relative value (i.e. transformed to a maximum of 1 or to a percentage),
#'  and does not correspond to quantum flux units ($umol*s^-1*m^-2$). Useful values
#'  are, for example, 500 (for dim light) and 10000 (for bright illumination). Note that if
#' \code{vonkries = TRUE} this transformation has no effect.
#'
#' @return An object of class \code{vismodel} containing the photon catches for each of the
#'  photoreceptors considered. Information on the parameters used in the calculation are also
#'  stored and can be called using the \code{summary.vismodel} function.
#'
#' @export
#'
#' @examples
#' # Dichromat (dingo)
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'canis')
#' di.flowers <- colspace(vis.flowers, space = 'di')
#'
#' # Trichromat (honeybee)
#' data(flowers)
#' vis.flowers <- vismodel(flowers, visual = 'apis')
#' tri.flowers <- colspace(vis.flowers, space = 'tri')
#'
#' # Tetrachromat (blue tit)
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis, visual = 'bluetit')
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#'
#' # Tetrachromat (starling), receptor-noise model
#' data(sicalis)
#' vis.star <- vismodel(sicalis, visual = 'star', achromatic = 'bt.dc', relative = FALSE)
#' dist.star <- coldist(vis.star, achromatic = TRUE)
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @references Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., & Cuthill, I.
#'  (1998). Tetrachromacy, oil droplets and bird plumage colours. Journal Of Comparative
#'  Physiology A-Neuroethology Sensory Neural And Behavioral Physiology, 183(5), 621-633.
#' @references Hart, N. S., Partridge, J. C., Cuthill, I. C., Bennett, A. T. D. (2000).
#' Visual pigments, oil droplets, ocular media and cone photoreceptor distribution in two
#' species of passerine bird: the blue tit (Parus caeruleus L.) and the blackbird
#' (Turdus merula L.). Journal of Comparative Physiology A, 186, 375-387.
#' @references Hart, N. S. (2001). The visual ecology of avian photoreceptors. Progress
#'  In Retinal And Eye Research, 20(5), 675-703.
#' @references Barbour H. R., Archer, M. A., Hart, N. S., Thomas, N., Dunlop, S. A.,
#'  Beazley, L. D, Shand, J. (2002). Retinal characteristics of the Ornate Dragon
#'  Lizard, Ctenophorus ornatus.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage
#'  color in a tetrahedral color space: A phylogenetic analysis of new world buntings.
#'  The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour patterns
#'  as birds see them. Biological Journal Of The Linnean Society, 86(4), 405-431.
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'    based on photoreceptor excitations as a generalized representation of
#'    colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Stockman, A., & Sharpe, L. T. (2000). Spectral sensitivities of
#'  the middle- and long-wavelength sensitive cones derived from measurements in
#'  observers of known genotype. Vision Research, 40, 1711-1737.
#' @references CIE (2006). Fundamental chromaticity diagram with physiological axes.
#'  Parts 1 and 2. Technical Report 170-1. Vienna: Central Bureau of the Commission
#'  Internationale de l' Eclairage.
#'

vismodel <- function(rspecdata,
                     visual = c(
                       "avg.uv", "avg.v", "bluetit", "ctenophorus", "star", "pfowl", "apis",
                       "canis", "cie2", "cie10", "musca", "segment", "habronattus", "rhinecanthus"
                     ),
                     achromatic = c("none", "bt.dc", "ch.dc", "st.dc", "md.r1", "ra.dc", "ml", "l", "all"),
                     illum = c("ideal", "bluesky", "D65", "forestshade"),
                     trans = c("ideal", "bluetit", "blackbird"),
                     qcatch = c("Qi", "fi", "Ei"),
                     bkg = c("ideal", "green"),
                     vonkries = FALSE, scale = 1, relative = TRUE) {

  # remove & save colum with wavelengths

  wl_index <- which(names(rspecdata) == "wl")
  wl <- rspecdata[, wl_index]
  y <- rspecdata[, -wl_index, drop = FALSE]

  # Negative value check
  if (any(y < 0)) {
    warning("The spectral data contain ", length(y[y < 0]),
            " negative value(s), which may produce unexpected results. ",
            "Consider using procspec() to correct them.")
  }

  visual2 <- tryCatch(
    match.arg(visual),
    error = function(e) "user-defined"
  )
  sens <- vissyst
  achromatic2 <- tryCatch(
    match.arg(achromatic),
    error = function(e) ifelse(isFALSE(achromatic), "none", "user-defined")
  )
  illum2 <- tryCatch(
    match.arg(illum),
    error = function(e) "user-defined"
  )
  bg2 <- tryCatch(
    match.arg(bkg),
    error = function(e) "user-defined"
  )
  if (is.null(bkg)) {
    stop("chosen background is NULL")
  }
  tr2 <- tryCatch(
    match.arg(trans),
    error = function(e) "user-defined"
  )
  if (is.null(trans)) {
    stop("chosen transmission is NULL")
  }

  qcatch <- match.arg(qcatch)

  # Model-specific defaults
  if (substr(visual2, 1, 3) == "cie") {
    if (!vonkries || relative || !identical(achromatic2, "none") || !identical(qcatch, "Qi")) {
      vonkries <- TRUE
      relative <- FALSE
      achromatic2 <- "none"
      qcatch <- "Qi"
      warning("cie system chosen, overriding incompatible parameters.", call. = FALSE)
    }
  }

  if (visual2 == "segment") {
    if (vonkries || !relative || !identical(achromatic2, "all") || identical(qcatch, "Qi") ||
      !identical(bg2, "ideal") || !identical(tr2, "ideal") || identical(illum2, "ideal")) {
      vonkries <- FALSE
      relative <- TRUE
      achromatic2 <- "all"
      qcatch <- "Qi"
      bg2 <- "ideal"
      tr2 <- "ideal"
      illum2 <- "ideal"
      warning("segment analysis chosen, overriding incompatible parameters.", call. = FALSE)
    }
  }

  # Grab the visual system
  if (visual2 == "segment") { # make a weird custom 'visual system' for segment analysis
    S <- data.frame(matrix(0, nrow = length(wl), ncol = 4))
    names(S) <- c("S1", "S2", "S3", "S4")
    segmts <- trunc(as.numeric(quantile(min(wl):max(wl))))

    S[wl %in% segmts[1]:segmts[2], 1] <- 1
    S[wl %in% segmts[2]:segmts[3], 2] <- 1
    S[wl %in% segmts[3]:segmts[4], 3] <- 1
    S[wl %in% segmts[4]:segmts[5], 4] <- 1

    sens_wl <- wl
  } else if (visual2 == "user-defined") {
    S <- visual[, names(visual) != "wl"]
    sens_wl <- visual[, "wl"]
    fullS <- visual
  } else {
    visual <- match.arg(visual)
    S <- sens[, grep(visual, names(sens))]
    names(S) <- gsub(paste0(visual, "."), "", names(S))
    sens_wl <- sens[, "wl"]
  }

  # Save cone numer
  ifelse(identical(visual2, "segment"),
    conenumb <- "seg",
    conenumb <- dim(S)[2]
  )

  # Check if wavelength range matches
  if (isFALSE(all.equal(wl, sens_wl, check.attributes = FALSE)) &
      visual2 == "user-defined") {
    stop(
      "wavelength range in spectra and visual system data do not match - ",
      "spectral data must range between 300 and 700 nm in 1-nm intervals.",
      "Consider interpolating using as.rspec()."
    )
  }

  if (isFALSE(all.equal(wl, sens_wl, check.attributes = FALSE))) {
    stop("wavelength range in spectra and visual system data do not match")
  }

  # DEFINING ILLUMINANT & BACKGROUND

  bgil <- bgandilum

  if (illum2 != "user-defined") {
    illum <- bgil[, grep(illum2, names(bgil))]
  }
  if (illum2 == "ideal") {
    illum <- rep(1, dim(rspecdata)[1])
  }

  if (bg2 != "user-defined") {
    bkg <- bgil[, grep(bg2, names(bgil))]
  }
  if (bg2 == "ideal") {
    bkg <- rep(1, dim(rspecdata)[1])
  }

  # Defining ocular transmission
  trdat <- transmissiondata

  if (tr2 != "user-defined") {
    trans <- trdat[, grep(tr2, names(trdat))]
  }
  if (tr2 == "ideal") {
    trans <- rep(1, dim(rspecdata)[1])
  }

  if (tr2 != "ideal" & visual == "user-defined") {
    if ("sensmod" %in% class(fullS)) {
      if (attr(fullS, "om")) {
        warning(
          "The visual system being used appears to already incorporate ocular ",
          'transmission. Using anything other than trans = "ideal", means ',
          "ocular media effects are being applied a second time.",
          call. = FALSE
        )
      }
    }
  }

  prepare_userdefined <- function(df) {
    if ("rspec" %in% class(df)) {
      dfwhichused <- names(df)[2]
      df <- df[, 2]
      warning(deparse(substitute(df)), " is an rspec object; first spectrum (",
        dQuote(dfwhichused), ") has been used (remaining columns ignored)",
        call. = FALSE
      )
    } else if ("data.frame" %in% class(df) | "matrix" %in% class(df)) {
      dfwhichused <- names(df)[1]
      df <- df[, 1]
      warning(deparse(substitute(df)), " is a matrix or data frame; first column (",
        dQuote(dfwhichused), ") has been used (remaining columns ignored)",
        call. = FALSE
      )
    }
    return(df)
  }

  trans      <- prepare_userdefined(trans)
  bkg        <- prepare_userdefined(bkg)
  illum      <- prepare_userdefined(illum)
  achromatic <- prepare_userdefined(achromatic)

  # Transform from percentages to proportions (Vorobyev 2003)
  if (max(y) > 1) {
    y <- y / 100
  }

  # Scale background from percentage to proportion
  if (max(bkg) > 1) {
    bkg <- bkg / 100
  }

  # Scale transmission from percentage to proportion
  if (max(trans) > 1) {
    trans <- trans / 100
  }

  # Scale illuminant
  illum <- illum * scale

  # Filter specs by transmission
  y <- y * trans

  # Model-specific modifiers, if need be
  B <- K <- 1
  if (substr(visual2, 1, 3) == "cie") {
    K <- 100 / colSums(S[2] * illum)
  }
  if (visual == "segment") {
    B <- apply(y, 2, sum)
  }

  # Calculate Qi
  Qi <- data.frame(
    crossprod(as.matrix(y), as.matrix(S * illum)) * B * K
  )

  # In case rspecdata only has one spectrum
  if (dim(Qi)[2] < 2) {
    Qi <- data.frame(t(Qi))
    rownames(Qi) <- names(y)
  }

  names(Qi) <- names(S)

  # Achromatic contrast

  # Calculate lum
  if (any(c("bt.dc", "ch.dc", "st.dc", "md.r1", "ra.dc", "ml", "l", "all", "user-defined") %in% achromatic2)) {
    L <- switch(achromatic2,
      "bt.dc" = ,
      "ch.dc" = ,
      "st.dc" = ,
      "md.r1" = ,
      "ra.dc" = sens[, grep(achromatic2, names(sens))],
      "ml" = rowSums(S[, c(dim(S)[2] - 1, dim(S)[2])]),
      "l" = S[, dim(S)[2]],
      "all" = rowSums(S),
      "user-defined" = achromatic
    )
    lum <- colSums(y * L * illum)
    Qi <- data.frame(cbind(Qi, lum))
  }

  if (achromatic2 == "segment") {
    Qi <- data.frame(cbind(Qi, B))
  }

  if (achromatic2 == "none") {
    L <- NULL
    lum <- NULL
  }

  # von Kries correction (constant adapting background)
  vk <- "(von Kries color correction not applied)"

  # Quantum catch normalized to the background (qi = k*Qi)
  if (vonkries) {
    if (!is.null(lum)) {
      S <- data.frame(cbind(S, L))
      uncqi <- Qi[, "lum"]
    }
    k <- 1 / (colSums(S * bkg * illum) * K)
    Qi <- data.frame(t(t(Qi) * k))
    vk <- "(von Kries color correction applied)"
    if (!is.null(lum)) {
      Qi[, "lum"] <- uncqi
    }
  }

  # Output
  res <- switch(qcatch,
                Qi = Qi,
                fi = log(Qi),
                Ei = Qi / (Qi + 1))

  # Convert to relative
  if (relative) {
    res[, !names(res) %in% "lum"] <- res[, !names(res) %in% "lum"] / rowSums(res[, !names(Qi) %in% "lum"])
  }

  class(res) <- c("vismodel", "data.frame")

  # Descriptive attributes
  attr(res, "qcatch") <- qcatch
  attr(res, "visualsystem.chromatic") <- visual2
  attr(res, "visualsystem.achromatic") <- achromatic2
  attr(res, "illuminant") <- paste0(illum2, ", scale = ", scale, " ", vk)
  attr(res, "background") <- bg2
  attr(res, "transmission") <- tr2
  attr(res, "relative") <- relative
  attr(res, "conenumb") <- conenumb
  attr(res, "vonkries") <- vonkries

  # Data attributes
  attr(res, "data.visualsystem.chromatic") <- S
  attr(res, "data.visualsystem.achromatic") <- L
  attr(res, "data.illuminant") <- illum
  attr(res, "data.background") <- bkg
  attr(res, "data.transmission") <- trans

  res
}
