#' Retrieve or plot in-built spectral sensitivity data
#'
#' Retrieve (as an rspec object) or plot pavo's in-built spectral sensitivity data.
#'
#' @param visual visual systems. Options are:
#' - `"none"`: no visual sensitivity data.
#' - `"all"`: all visual sensitivity data.
#' - `"apis"`: Honeybee *Apis mellifera* visual system.
#' - `"avg.uv"`: average avian UV system.
#' - `"avg.v"`: average avian V system.
#' - `"bluetit"`: Blue tit *Cyanistes caeruleus* visual system.
#' - `"canis"`: Canid *Canis familiaris* visual system.
#' - `"cie2"`: 2-degree colour matching functions for CIE models of human
#'  colour vision. Functions are linear transformations of the 2-degree cone fundamentals
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' - `"cie10"`: 10-degree colour matching functions for CIE models of human
#'  colour vision. Functions are linear transformations of the 10-degree cone fundamentals
#'  of Stockman & Sharpe (2000), as ratified by the CIE (2006).
#' - `"ctenophorus"`: Ornate dragon lizard *Ctenophorus ornatus*.
#' - `"musca"`: Housefly *Musca domestica* visual system.
#' - `"pfowl"`: Peafowl *Pavo cristatus* visual system.
#' - `"star"`: Starling *Sturnus vulgaris* visual system.
#' - `"habronattus"`: Jumping spider *Habronattus pyrrithrix*.
#' - `"rhinecanthus"`: Triggerfish *Rhinecanthus aculeatus*.
#' @param achromatic the sensitivity data used to calculate luminance (achromatic)
#'  receptor stimulation. Options are:
#' - `"none"`: no achromatic sensitivity data.
#' - `"all"`: all achromatic sensitivity data.
#' - `"bt.dc"`: Blue tit *Cyanistes caeruleus* double cone.
#' - `"ch.dc"`: Chicken *Gallus gallus* double cone.
#' - `"st.dc"`: Starling *Sturnus vulgaris* double cone.
#' - `"cf.r"`: Canid *Canis familiaris* rod
#' - `"md.r1"`: Housefly *Musca domestica* R1-6 photoreceptor.
#' - `"ra.dc"`: Triggerfish *Rhinecanthus aculeatus* double cone.
#' @param illum illuminants. Options are:
#' - `"none"`: no illuminant data.
#' - `"all"`: all background spectral data.
#' - `"bluesky"` open blue sky.
#' - `"D65"`: standard daylight.
#' - `"forestshade"` forest shade.
#' @param bkg background spectra. Options are:
#' - `"none"`: no background spectral data.
#' - `"all"`: all background spectral data.
#' - `"green"`: green foliage.
#' @param trans Ocular transmission data. Options are:
#' - `"none"`: no transmission data.
#' - `"all"`: all transmission data.
#' - `"bluetit"`: blue tit *Cyanistes caeruleus* ocular transmission (from Hart et al. 2000).
#' - `"blackbird"`: blackbird *Turdus merula* ocular transmission (from Hart et al. 2000).
#' @param plot should the spectral data be plotted, or returned instead (defaults to `FALSE`)?
#' @param ... additional graphical options passed to [plot.rspec()] when `plot = TRUE`.
#'
#' @return An object of class `rspec` (when `plot = FALSE`), containing
#' a wavelength column `"wl"` and spectral data binned at 1 nm intervals from 300-700 nm.
#'
#' @examples
#' # Plot the honeybee's receptors
#' sensdata(visual = "apis", ylab = "Absorbance", plot = TRUE)
#'
#' # Plot the average UV vs V avian receptors
#' sensdata(visual = c("avg.v", "avg.uv"), ylab = "Absorbance", plot = TRUE)
#'
#' # Retrieve the CIE colour matching functions as an rspec object
#' ciedat <- sensdata(visual = c("cie2", "cie10"))
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @export
#'

sensdata <- function(
                     visual = c(
                       "none", "all", "avg.uv", "avg.v", "bluetit", "ctenophorus", "star",
                       "pfowl", "apis", "canis", "cie2", "cie10", "musca", "habronattus", "rhinecanthus"
                     ),
                     achromatic = c("none", "all", "bt.dc", "ch.dc", "st.dc", "md.r1", "ra.dc", "cf.r"),
                     illum = c("none", "all", "bluesky", "D65", "forestshade"),
                     trans = c("none", "all", "bluetit", "blackbird"),
                     bkg = c("none", "all", "green"),
                     plot = FALSE,
                     ...) {
  visual2 <- match.arg(visual, several.ok = TRUE)
  achro2 <- match.arg(achromatic, several.ok = TRUE)
  illum2 <- match.arg(illum, several.ok = TRUE)
  bkg2 <- match.arg(bkg, several.ok = TRUE)
  trans2 <- match.arg(trans, several.ok = TRUE)

  dat <- data.frame(wl = 300:700)

  # Visual system
  if (!isTRUE("none" %in% visual2)) {
    if (isTRUE("all" %in% visual2)) {
      visual2 <- c(
        "avg.uv", "avg.v", "bluetit", "star", "pfowl", "apis",
        "canis", "cie2", "cie10", "musca", "habronattus", "rhinecanthus",
        "ctenophorus"
      )
    }
    sens <- as.data.frame(vissyst)
    S <- as.data.frame(subset(sens, select = grepl(paste(visual2, collapse = "|"), names(sens))))
    dat <- cbind(dat, S)
  }

  # Achromatic receptor
  if (!isTRUE("none" %in% achro2)) {
    if (isTRUE("all" %in% achro2)) {
      achro2 <- c("bt.dc", "ch.dc", "st.dc", "md.r1", "ra.dc", "cf.r")
    }
    sens <- as.data.frame(vissyst)
    achro <- as.data.frame(subset(sens, select = grepl(paste(achro2, collapse = "|"), names(sens))))
    dat <- cbind(dat, achro)
  }

  # Illuminant
  if (!isTRUE("none" %in% illum2)) {
    if (isTRUE("all" %in% illum2)) {
      illum2 <- c("bluesky", "D65", "forestshade")
    }
    bgil <- as.data.frame(bgandilum)
    illum <- as.data.frame(subset(bgil, select = grepl(paste(illum2, collapse = "|"), names(bgil))))
    dat <- cbind(dat, illum)
  }

  # Background
  if (!isTRUE("none" %in% bkg2)) {
    if (isTRUE("all" %in% bkg2)) {
      bkg2 <- "green"
    }
    bgil <- as.data.frame(bgandilum)
    bkg <- as.data.frame(subset(bgil, select = grepl(paste(bkg2, collapse = "|"), names(bgil))))
    dat <- cbind(dat, bkg)
  }

  # Transmission
  if (!isTRUE("none" %in% trans2)) {
    if (isTRUE("all" %in% trans2)) {
      trans2 <- c("bluetit", "blackbird")
    }
    trdat <- as.data.frame(transmissiondata)
    trans <- as.data.frame(subset(trdat, select = grepl(paste(trans2, collapse = "|"), names(trdat))))
    dat <- cbind(dat, trans)
  }

  dat <- suppressMessages(as.rspec(dat))

  if (plot == TRUE) {
    plot(dat, ...)
  } else {
    dat
  }
}
