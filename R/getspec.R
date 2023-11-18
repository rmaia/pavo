#' Import spectra files
#'
#' Finds and imports spectra files from a folder. Currently works
#' for reflectance files generated in Ocean Optics SpectraSuite (USB2000,
#' USB4000 and Jaz spectrometers), CRAIC software (after exporting) and
#' Avantes (before or after exporting).
#'
#' @inheritParams lightr::lr_get_spec
#'
#' @inherit lightr::lr_get_spec details
#'
#' @return A data frame, of class `rspec`, containing individual imported
#' spectral files as columns.
#' Reflectance values are interpolated to the nearest wavelength integer.
#'
#' @export
#'
#' @importFrom lightr lr_get_spec
#'
#' @seealso [lightr::lr_get_spec()] for a more flexible version of this function
#' (e.g. uninterpolated wavelengths), and [lightr::lr_get_metadata()] for the retrieval
#' and import of spectral metadata.
#' See <https://docs.ropensci.org/lightr/> for the complete, and up-to-date, list
#' of supported file formats.
#'
#' @examples
#' # Import and inspect example spectral data with a range of set to 400-700nm.
#' rspecdata <- getspec(system.file("testdata", package = "lightr"), ext = "ttt", lim = c(400, 700))
#' head(rspecdata)
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#'
#' @references Gruson H, White TE, Maia R (2019) lightr: import spectral data
#'  and metadata in R. Journal of Open Source Software, 4(43), 1857,
#'  \doi{doi:10.21105/joss.01857}.

getspec <- function(where = getwd(), ext = "txt", lim = c(300, 700), decimal = ".",
                    sep = NULL, subdir = FALSE, subdir.names = FALSE,
                    ignore.case = TRUE) {
  lr_get_spec(
    where = where, ext = ext, lim = lim, decimal = decimal, sep = sep,
    subdir = subdir, subdir.names = subdir.names,
    ignore.case = ignore.case
  )
}
