#' Convert data to an rspec object
#'
#' Converts data frames or matrices containing spectral data to \code{rspec} object
#'
#' @param object (required) a data frame or matrix containing spectra to process
#' @param whichwl specifies which column contains wavelengths. If NULL (default), function
#' searches for column containing equally spaced numbers and sets it as wavelengths "wl". If no
#' wavelengths are found or \code{whichwl} is not given, returns arbitrary index values
#' @return an object of class \code{rspec} for use in further \code{pavo} functions
#' @export
#' @examples \dontrun{
#'
#' # Generate some fake reflectance data
#' fakedat <- data.frame(refl1 = rnorm(401), refl2 = rnorm(401), wavelength = c(300:700))
#' head(fakedat)
#'
#' # Determine if is rspec object
#' is.rspec(fakedat)
#'
#' # Convert to rspec object
#' fakedat2 <- as.rspec(fakedat)
#' is.rspec(fakedat2)
#' head(fakedat2)}
#''
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

as.rspec <- function(object, whichwl = NULL) {

if (is.matrix(object)) {
  name <- colnames(object)
} else
if (is.data.frame(object)) {
  name <- names(object)
} else {
  stop('object must be a data frame or matrix')
}


ind <- sapply(1:ncol(object), function(x) {sd(diff(object[,x]))})

if (any(ind==0)) {
  wl_index <- which(ind==0)
  wl <- object[, wl_index]
  object <- object[, -wl_index]
  name <- name[-wl_index]
} else {
  wl <- 1:nrow(object)
  object <- object
  name <- name
  warning('No wavelengths or whichwl provided; using arbitrary index values')
}

res <- as.data.frame(cbind(wl, object))

names(res) <- c('wl', name)

class(res) <- c('rspec', 'data.frame')

res

}

#' @rdname as.rspec
#' @return a logical value indicating whether the object is of class \code{rspec}

is.rspec <- function(object) inherits(object, "rspec")
