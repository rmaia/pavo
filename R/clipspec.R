#' Clip artifacts from spectra
#' 
#' Cuts out portions of spectra that may have been identified as containing
#' artifactual reflectance values.
#' 
#' @inheritParams aggplot
#' 
#' @param from,to range of wavelengths to clip out. This range is exclusive,
#' meaning that wavelengths equal to `from` or `to` will not be clipped. `to`
#' must be greater or equal to `from`.
#' 
#' @param interpolate (logical) whether to linearly interpolate reflectance
#' values across missing wavelengths once the latter have been removed. If
#' FALSE (default), missing wavelengths are not replaced.
#' 
#' @return A data frame of class `rspec` with the clipped data.
#' 
#' @details The interpolation step internally calls the `as.rspec()` function
#' with its `interp` argument set to TRUE.
#' 
#' @note Preferably use this function before `procspec()`, otherwise artifacts
#' may affect the smoothing.
#' 
#' @export
#' 
#' @author Raphaël Scherrer (\email{raphael.scherrer@@evobio.eu})
#' 
#' @examples
#' 
#' # Load data
#' data(sicalis)
#' 
#' # Eyeball
#' plot(sicalis, select = 10:14)
#' 
#' # Remove the bump around 470nm
#' sicalis <- clipspec(sicalis, from = 460, to = 480)
#' 
#' # Check again
#' plot(sicalis, select = 10:14)
#' 
#' @seealso [procspec()], [as.rspec()]

# Function to clip a segment out of spectra
clipspec <- function(rspecdata, from, to, interpolate = FALSE) {
  
  # Check
  if (!is.rspec(rspecdata)) if (!is.data.frame(rspecdata)) stop("rspecdata must be an rspec object or at least a data frame")
  if (!is.numeric(from)) stop("from must be numeric")
  if (!is.numeric(to)) stop("to must be numeric")
  if (to < from) stop("from must be smaller than or equal to to")
  if (!is.logical(interpolate)) stop("interpolate must be logical")
  
  # Check
  if (!("wl" %in% colnames(rspecdata))) stop("column wl not found")
  
  # Extract wavelengths
  wl <- rspecdata[["wl"]]
  
  # Check
  if (!is.numeric(wl)) stop("wavelengths must be numeric")
  
  # Identify rows to keep
  ii <- wl < to & wl > from
  
  # Clip
  rspecdata <- rspecdata[!ii,]
  
  # Interpolate if needed
  if (interpolate) {
    clipped_wl <- wl[!ii]
    rspecdata <- vapply(
      isolate_wl(rspecdata, keep = "spec"),
      function(spec) {
        approx(x = clipped_wl, y = spec, xout = wl, rule = 2)$y
      },
      numeric(length(wl))
    )
  }
  
  # Exit
  return(rspecdata)
  
}