#' Peak shape descriptors
#'
#' Calculates height, location and width of peak at the reflectance midpoint (FWHM).
#' Note: bounds should be set wide enough to incorporate all minima in spectra. Smoothing
#' spectra using [procspec()] is also recommended.
#'
#' @param lim a vector specifying the wavelength range to analyze.
#' @param plot logical. Should plots indicating calculated parameters be returned?
#' (Defaults to `TRUE`).
#' @param ask logical, specifies whether user input needed to plot multiple plots
#' when number of spectra to analyze is greater than 1 (defaults to `FALSE`).
#' @param absolute.min logical. If `TRUE`, full width at half maximum will be
#' calculated using the absolute minimum reflectance of the spectrum, even if
#' that value falls outside the range specified by `lim`. (defaults to `FALSE`)
#' @param ... additional arguments to be passed to plot.
#' @inheritParams plotsmooth
#' @inheritParams plot.rspec
#'
#' @return a data frame containing column names (id); peak height (max value, B3), location (hue, H1) and full width
#' at half maximum (FWHM), as well as half widths on left (HWHM.l) and right side of peak (HWHM.r). Incl.min column
#' indicates whether user-defined bounds incorporate the actual minima of the spectra.
#' Function will return a warning if not.
#'
#' @seealso [procspec()]
#'
#' @export
#'
#' @examples
#' data(teal)
#'
#' peakshape(teal, select = 3)
#' peakshape(teal, select = 10)
#'
#' # Use wavelength bounds to narrow in on peak of interest
#' peakshape(teal, select = 10, lim = c(400, 550))
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}

peakshape <- function(rspecdata, select = NULL, lim = NULL,
                      plot = TRUE, ask = FALSE, absolute.min = FALSE, ...) {

  wl <- isolate_wl(rspecdata, keep = "wl")

  if (is.null(select)) {
    rspecdata <- isolate_wl(rspecdata, keep = "spec")
  } else {
    rspecdata <- isolate_wl(rspecdata[, select, drop = FALSE], keep = "spec")
  }
  nms <- names(rspecdata)
  # set default wavelength range if not provided
  if (is.null(lim)) {
    lim <- range(wl)
  }

  if (ncol(rspecdata) == 0) {
    return(NULL)
  }

  wlrange <- seq(lim[1], lim[2])

  rspecdata2 <- rspecdata[wl >= lim[1] & wl <= lim[2], , drop = FALSE]

  Bmax <- apply(rspecdata2, 2, max) # max refls
  Bmin <- apply(rspecdata2, 2, min) # min refls
  Bmin_all <- apply(rspecdata, 2, min) # min refls, whole spectrum

  if (absolute.min) {
    halfmax <- (Bmax + Bmin_all) / 2
  } else {
    halfmax <- (Bmax + Bmin) / 2
  }

  Xi <- lapply(
    seq_along(rspecdata2),
    function(x) which(rspecdata2[, x] == Bmax[x])
  ) # lambda_max index

  dblpeaks <- vapply(Xi, length, numeric(1))
  if (any(dblpeaks > 1)) {
    # Keep only first peak of each spectrum
    dblpeak_nms <- nms[dblpeaks > 1]
    warning("Multiple wavelengths have the same reflectance value (",
      paste(dblpeak_nms, collapse = ", "), "). Using first peak found. ",
      "Please check the data or try smoothing.",
      call. = FALSE
    )
  }
  Xi <- apply(rspecdata2, 2, which.max)

  hilo <- t(t(rspecdata2) - halfmax) < 0

  FWHM_lims <- sapply(seq_len(ncol(rspecdata2)), function(x) {
    # Start at H1 and find first value below halfmax
    fstHM <- match(TRUE, hilo[seq(Xi[x], 1, -1), x])
    sndHM <- match(TRUE, hilo[Xi[x]:nrow(rspecdata2), x])
    return(c(fstHM, sndHM))
  })

  if (any(Bmin > Bmin_all)) {
    warning("Consider fixing ", dQuote("lim"), " in spectra with ",
      dQuote("incl.min"), " marked ", dQuote("No"),
      " to incorporate all minima in spectral curves",
      call. = FALSE
    )
  }

  hue <- wlrange[Xi]

  # Shift FWHM_lims by 1 because we calculated the index by including H1.
  Xa <- wlrange[Xi - FWHM_lims[1, ] + 1]
  Xb <- wlrange[Xi + FWHM_lims[2, ] - 1]

  if (plot) {
    oPar <- par("ask")
    on.exit(par(oPar))
    par(ask = ask)

    for (i in seq_along(rspecdata)) {
      plot(rspecdata[, i] ~ wl,
        type = "l", xlab = "Wavelength (nm)",
        ylab = "Reflectance (%)", main = nms[i], ...
      )
      abline(v = hue[i], col = "red")
      abline(h = halfmax[i], col = "red")
      abline(v = Xa[i], col = "red", lty = 2)
      abline(v = Xb[i], col = "red", lty = 2)
      abline(v = lim, col = "lightgrey")
    }
  }

  data.frame(
    id = nms, B3 = as.numeric(Bmax), H1 = hue,
    FWHM = Xb - Xa, HWHM.l = hue - Xa, HWHM.r = Xb - hue,
    incl.min = c("Yes", "No")[as.numeric(Bmin > Bmin_all) + 1]
  )
}
