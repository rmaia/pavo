#' Convert data to an rspec object
#'
#' Converts data frames or matrices containing spectral data to \code{rspec} object
#'
#' @param specs (required) an \code{rspec} object containing spectra to process
#' @param whichwl specifies which column contains wavelengths. If NULL (default), function
#' searches for column titled "wl" or else returns arbitrary index values
#' @return an object of class \code{rspec} for use in further \code{pavo} functions
#' @export
#' @examples \dontrun{
#' #INCLUDE EXAMPLE}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

as.rspec <- function(specs, whichwl = NULL) {

if (is.matrix(specs)) {
  name <- colnames(specs)
}
if (is.data.frame(specs)) {
  name <- names(specs)
} else {
stop('object must be a data frame or matrix')
}

# make wavelength vector
wl_index <- which(colnames(specs)=='wl'||names(specs)=='wl')

if (length(wl_index) > 0) {
  wl <- specs[, wl_index]
  specs <- specs[, -wl_index]
  name <- name[-wl_index]
} else if (!is.null(whichwl)) {
  wl <- specs[, whichwl]
  specs <- specs[, -whichwl]
  name <- name[-whichwl]
} else {
  wl <- 1:nrow(specs)
  specs <- specs
  name <- name
  warning('No wavelengths or whichwl provided; using arbitrary index values')
}

res <- as.data.frame(cbind(wl, specs))

names(res) <- c('wl', name)

class(res) <- c('rspec', 'data.frame')

res

}
