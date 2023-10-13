#' Stitch together two rspec objects
#'
#' Stitch (row-wise merge) two `rspec` objects of differing wavelength ranges into
#' a single `rspec` object.
#'
#' @param rspec1,rspec2 (required) `rspec` objects of differing wavelength ranges
#' to stitch together by row.
#' @param overlap_method the method for modifying reflectance values if regions
#' of the spectra overlap in their wavelength range. Defaults to `mean`.
#' @param interp logical argument specifying whether reflectance values should be
#' interpolated between the two sets of spectra if their wavelength ranges
#' do not overlap. Defaults to `TRUE`.
#'
#' @export
#'
#' @examples
#'
#' # Simulate a UV-VIS and NIR reflectance spectrum whose wavelength regions
#' # slightly overlap then stitch them together, with the overlapping
#' # regions being averaged.
#'
#' # Simulate specs
#' reflect1 <- simulate_spec(wl_peak = 550, xlim = c(300, 700))
#' reflect2 <- simulate_spec(wl_inflect = 1100, xlim = c(650, 1200))
#'
#' # Ensure the names of the spectra match
#' names(reflect1) <- names(reflect2) <- c('wl', 'sample_1')
#'
#' # Stitch the spectra together by their wavelength column
#' full_spec <- stitch(reflect1, reflect2)
#'
#' # Plot the resulting spectrum
#' plot(full_spec)
#'
#' # Simulate another set of UV-VIS and NIR spectra. Note two additional complexities,
#' # both of which are handled without issue. First, the wavelength ranges are
#' # non-overlapping (with a 100 nm gap). We'll keep the default interp = TRUE argument
#' # to allow the missing reflectance region to be interpolated. Second, the names of
#' # the spectra match, but are in a different order in the two rspec objects. This isn't
#' # an issue, as the function can match up the spectra by name irrespective of their
#' # ordering
#'
#' # Simulate UV-VIS and NIR spectra
#' reflect_vis <- merge(simulate_spec(wl_peak = 550, xlim = c(300, 700)),
#'                      simulate_spec(wl_peak = 550, xlim = c(300, 700)))
#' reflect_nir <- merge(simulate_spec(wl_inflect = 1000, xlim = c(800, 1250)),
#'                      simulate_spec(wl_inflect = 1100, xlim = c(800, 1250)))
#'
#' # Ensure the names of the spectra exist in each, albeit in a different order
#' names(reflect_vis) <- c('wl', 'sample_1', 'sample_2')
#' names(reflect_nir) <- c('wl', 'sample_2', 'sample_1')
#'
#' # Stitch together by their wavelength column, with missing regions being
#' # interpolated
#' reflect_vis_nir <- stitch(reflect_vis, reflect_nir)
#'
#' # Plot the resulting spectrum
#' plot(reflect_vis_nir)
#'
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#'
#' @seealso [as.rspec()], [merge.rspec()]

stitch <- function(rspec1, rspec2, overlap_method, interp) {
  UseMethod("stitch")
}

#' @rdname stitch
#'
#' @export
stitch.rspec <- function(rspec1, rspec2,
                                 overlap_method = c("mean", "minimum", "maximum"),
                                 interp = TRUE) {

  # Class check
  if (!inherits(rspec1, "rspec") || !inherits(rspec2, "rspec")) {
    stop("Both inputs must be of class 'rspec'")
  }

  # Validate overlap_method
  overlap_method <- match.arg(overlap_method)

  # Check that at least one spectrum has a matching name in both objects
  common_cols <- intersect(names(rspec1), names(rspec2))
  if (length(common_cols) <= 1) {
    stop("At least one spectrum in both rspec objects must have a matching name")
  }

  # Warn if only subset is present across both rspec objects
  if (length(common_cols) != ncol(rspec1) || length(common_cols) != ncol(rspec2)) {
    warning("Not all spectra are present in both objects. Stitching only the common samples.")
  }

  # Identify unique spectra in both objects
  unique_rspec1 <- setdiff(names(rspec1), common_cols)
  unique_rspec2 <- setdiff(names(rspec2), common_cols)

  # Create NA-filled columns in each rspec for unique spectra in the other
  rspec1[, unique_rspec2] <- NA
  rspec2[, unique_rspec1] <- NA

  # Reorder columns of rspec2 to match rspec1
  rspec2 <- rspec2[, names(rspec1)]

  # Merge by wl
  res <- rbind(rspec1, rspec2)

  # Handle overlapping wl values
  overlap_wl <- res$wl[duplicated(res$wl)]

  for (wl in overlap_wl) {
    idx <- which(res$wl == wl)

    # Replace with a switch statement
    res[idx[1], -1] <- switch(
      overlap_method,
      mean =  colMeans(as.matrix(res[idx, -1]), na.rm = TRUE),
      minimum = apply(as.matrix(res[idx, -1]), 2, min, na.rm = TRUE),
      maximum = apply(as.matrix(res[idx, -1]), 2, max, na.rm = TRUE)
    )

    # Remove extra rows
    res <- res[-idx[2], ]
  }

  # Interpolate missing values
  if (interp) {
    full_wl_range <- min(res$wl):max(res$wl)
    missing_wl <- setdiff(full_wl_range, res$wl)

    if (length(missing_wl) > 0) {
      # Interpolate only common spectra
      new_rows <- as.data.frame(vapply(
        common_cols[-1],
        function(i) {
          approx(res$wl, res[, i], xout = missing_wl)$y
        },
        FUN.VALUE = numeric(length(missing_wl))
      ))
      new_rows$wl <- missing_wl

      res <- rbind(res, new_rows)
    }
  }

  # Sort stitched spec by wl
  res <- res[order(res$wl), ]

  # Classes
  class(res) <- c("rspec", "data.frame")

  res
}
