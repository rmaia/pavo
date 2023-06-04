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
#' curve. Defaults to 0 - 100 % (`c(0, 100)`).
#'
#' @return An object of class `rspec`.
#' 
#' @examples
#' # Single sigmoidal spectrum, with an inflection at 500 nm.
#' spec <- as.rspec(simulate_spec2(wl_inflect = 500))
#'
#' # Single Gaussian spectrum, with a peak at 400 nm
#' spec2 <- as.rspec(simulate_spec2(wl_peak = 400))
#'
#' # Combination of both Gaussian (with peak at 340 nm) and sigmoidal (with inflection
#' # at 560 nm)
#' spec3 <- as.rspec(simulate_spec2(wl_peak = 340, wl_inflect = 560))
#'
#' # Double-Gaussian peaks of differing widths
#' spec4 <- as.rspec(simulate_spec2(wl_peak = c(340, 560), width_gauss = c(12, 40)))
#'
#' # Complex spectrim with multi-Gaussian and single sigmoidal peak
#' spec5 <- as.rspec(simulate_spec2(wl_peak = c(340, 430), width_gauss = c(20, 60), wl_inflect = 575)) 
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
  
  # Error handling
  if (is.null(wl_inflect) && is.null(wl_peak)) {
    stop("Either wl_inflect or wl_peak must be specified")
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
  
  # Generate wavelengths from the specified range
  wl <- seq(xlim[1], xlim[2], by = 1)
  
  # Initialize the reflectance vector
  spec <- vector("numeric", length(wl))
  
  # Convert FWHM to standard deviation for Gaussian function
  sigma.gauss <- width_gauss / (2 * sqrt(2 * log(2)))
  
  # Simulate the specs
  # Sigmoidal
  if (!is.null(wl_inflect)) {
    spec.sig <- mapply(function(wli, wsi) {
      (ylim[2] - ylim[1]) / (1 + exp(-(wl - wli)/wsi)) + ylim[1]
    }, wl_inflect, width_sig)
    spec <- spec + rowSums(spec.sig, na.rm = TRUE)
  }
  
  # Gaussian
  if (!is.null(wl_peak)) {
    spec.gauss <- mapply(function(wlp, wga) {
      (ylim[2] - ylim[1]) * exp(-((wl - wlp)^2)/(2 * wga^2)) + ylim[1]
    }, wl_peak, sigma.gauss)
    spec <- spec + rowSums(spec.gauss, na.rm = TRUE)
  }
  
  # Normalize the final spectrum to span the range specified by ylim
  spec <- (spec - min(spec)) / (max(spec) - min(spec))  # Scale to [0, 1]
  spec <- spec * (ylim[2] - ylim[1]) + ylim[1]  # Rescale to ylim
  
  # Final
  spec_df <- as.data.frame(cbind(wl, spec))
  class(spec_df) <- c("rspec", "data.frame")
  return(spec_df)
}