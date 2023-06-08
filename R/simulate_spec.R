#' Simulate a reflectance spectrum
#'
#' Simulate a natural reflectance spectrum with sigmoidal (s-shaped) and/or Gaussian
#' (bell-shaped) curves. Multiple Gaussian and sigmoidal curves can be combined in
#' a single spectrum, to simulate more complex reflectance functions.
#'
#' @param wl_inflect A numeric vector specifying the wavelength location (in nm)
#' for one or more inflection point(s) for a 'sigmoid' shaped curve, if desired.
#' @param wl_peak A numeric vector specifying the wavelength location (in nm)
#' for one or more inflection point(s) for a 'Gaussian' (or 'bell') shaped curve,
#' if desired.
#' @param width_sig A numeric value or vector (if multiple wl_inflect values are specified)
#' specifying the steepness of the change, for any sigmoidal curve(s). Required when
#' `wl_peak` is specified. Defaults to 20 nm.
#' @param width_gauss A numeric value or vector specifying the the full-width at half-maximum
#' of any Gaussian curve(s). Required when `wl_peak` is specified. Defaults to 70 nm.
#' @param xlim A vector specifying the wavelength range of the simulated spectra. Defaults to
#' 300-700nm (`c(300, 700)`).
#' @param ylim A vector specifying the minimum and maximum reflectance values of the resulting
#' curve. Defaults to 0 - 100 % (`c(0, 100)`). Note: sigmoidal curves, by default, move 
#' from low to high reflectance. But if high-to-low sigmoidal curves are desired this
#' can be controlled by the ordering of the values given to `ylim()`. E.g. `c(0, 100)`
#' will generate a low-to-high sigmoidal curve, whereas `c(100, 0)` will generate a high-
#' to-low curve. The ordering of values has no effect on the Gaussian portions of the 
#' final curve. 
#'
#' @return An object of class `rspec`.
#'
#' @examples
#' # Single ideal 'grey' reflectance spectrum, with 50% reflectance across 300 - 700 nm.
#' spec0 <- simulate_spec(ylim = c(0, 50))
#'
#' # Single sigmoidal spectrum, with a low-to-high inflection at 550 nm.
#' spec1 <- simulate_spec(wl_inflect = 550)
#'
#' # Single Gaussian spectrum, with a peak at 400 nm
#' spec2 <- simulate_spec(wl_peak = 400)
#'
#' # Combination of both Gaussian (with peak at 340 nm) and sigmoidal (with inflection
#' # at 560 nm)
#' spec3 <- simulate_spec(wl_inflect = 560, wl_peak = 340)
#'
#' # Double-Gaussian peaks of differing widths
#' spec4 <- simulate_spec(wl_peak = c(340, 560), width_gauss = c(12, 40))
#'
#' # Complex spectrim with single sigmoidal peak and multi-Gaussian peaks
#' spec5 <- simulate_spec(wl_inflect = 575, wl_peak = c(340, 430), width_gauss = c(20, 60))
#'
#' # Simulate a set of Gaussian reflectance curves with peaks varying between 400-600nm 
#' # in increments of 10, then combine into a single rspec object, and plot the result
#' peaks <- seq(400, 600, 10)  # Peak locations
#' spectra <- lapply(seq_along(peaks), function(x) simulate_spec(wl_peak = peaks[x]))  # Simulate
#' spectra <- Reduce(function(...) merge(..., all = TRUE), spectra)  # Combine
#' plot(spectra)  # Plot
#'
#' # Simulate a set of Gaussian reflectance curves with a single peak at 500 nm, but
#' # with maximum reflectance varying from 30 to 90% in 10% increments, then combine 
#' # into a single rspec object, and plot the result
#' ymax <- lapply(seq(30, 90, 10), function(x) c(0, x))  # Varying reflectance maxima
#' spectra2 <- lapply(ymax, function(x) simulate_spec(wl_peak = 500, ylim = x))  # Simulate
#' spectra2 <- Reduce(function(...) merge(..., all = TRUE), spectra2)  # Combine
#' plot(spectra2)  # Plot
#'
#' @export
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#'
simulate_spec <- function(wl_inflect = NULL, 
                          wl_peak = NULL, 
                          width_sig = 20,
                          width_gauss = 70,
                          xlim = c(300, 700), 
                          ylim = c(0, 100)) {

  # Check ylim ordering and set flag for high-to-low sigmoidal curve
  sig_hightolow <- FALSE
  if (ylim[1] > ylim[2]) {
    sig_hightolow <- TRUE
  }
  
  # Generate wavelengths from the specified range
  wl <- seq(xlim[1], xlim[2], by = 1)

  # Initialize the reflectance vector
  spec <- vector("numeric", length(wl))

  # When neither wl_inflect nor wl_peak are specified, return ideal 100% reflectance
  if (is.null(wl_inflect) && is.null(wl_peak)) {
    spec <- rep(ylim[2], length(wl))
    spec_df <- as.data.frame(cbind(wl, spec))
    names(spec_df)[2] <- paste0("spec_ideal-", ylim[2])
    class(spec_df) <- c("rspec", "data.frame")
    return(spec_df)
  }

  # Check that the size of wl_inflect and width_sig match, or width_sig is a single value
  # https://vctrs.r-lib.org/reference/vector_recycling_rules.html
  if (!is.null(wl_inflect) && length(width_sig) != 1 && length(wl_inflect) != length(width_sig)) {
    stop("Size of wl_inflect and width_sig must match, or width_sig must be a single value")
  }

  # Check that the size of wl_peak and width_gauss match, or width_gauss is a single value
  if (!is.null(wl_peak) && length(width_gauss) != 1 && length(wl_peak) != length(width_gauss)) {
    stop("Size of wl_peak and width_gauss must match, or width_gauss must be a single value")
  }

  # Convert FWHM to standard deviation for Gaussian function
  sigma.gauss <- width_gauss / (2 * sqrt(2 * log(2)))

  # Simulate the specs
  # Sigmoidal
  if (!is.null(wl_inflect)) {
    spec.sig <- mapply(function(wli, wsi) {
      if(sig_hightolow) {
        (ylim[1] - ylim[2]) / (1 + exp(-(wl - wli) / wsi)) + ylim[2]
      } else {
        (ylim[2] - ylim[1]) / (1 + exp(-(wl - wli) / wsi)) + ylim[1]
      }
    }, wl_inflect, width_sig)
    spec <- spec + rowSums(spec.sig, na.rm = TRUE)
  }

  # Gaussian
  if (!is.null(wl_peak)) {
    spec.gauss <- mapply(function(wlp, wga) {
      (ylim[2] - ylim[1]) * exp(-((wl - wlp)^2) / (2 * wga^2)) + ylim[1]
    }, wl_peak, sigma.gauss)
    spec <- spec + rowSums(spec.gauss, na.rm = TRUE)
  }

  # Normalize the final spectrum to span the range specified by ylim
  spec <- (spec - min(spec)) / (max(spec) - min(spec))  # Scale to [0, 1]
  spec <- spec * (ylim[2] - ylim[1]) + ylim[1]  # Rescale to ylim

  # Construct unique name for the spectrum
  name <- "spec_"
  # If sigmoidal, append `i-` followed by inflection point locations
  if (!is.null(wl_inflect)) {
    name <- paste0(name, "i-", paste(wl_inflect, collapse = "-"))
  }
  # If gaussian, append `p-` followed by peak locations
  if (!is.null(wl_peak)) {
    if (!is.null(wl_inflect)) name <- paste0(name, "_")  # Also add separator if both specified
    name <- paste0(name, "p-", paste(wl_peak, collapse = "-"))
  }
  
  # Final output
  spec_df <- as.data.frame(cbind(wl, spec))
  names(spec_df)[2] <- name
  class(spec_df) <- c("rspec", "data.frame")
  return(spec_df)
}
