#' Convert data to an rspec object
#'
#' Converts data frames or matrices containing spectral data to \code{rspec} object
#'
#' @param object (required) a data frame or matrix containing spectra to process
#' @param whichwl specifies which column contains wavelengths. If NULL (default), function
#' searches for column containing equally spaced numbers and sets it as wavelengths "wl". If no
#' wavelengths are found or \code{whichwl} is not given, returns arbitrary index values
#' @param interp whether to interpolate wavelengths in 1-nm bins
#' @return an object of class \code{rspec} for use in further \code{pavo} functions
#' @export as.rspec is.rspec
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
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

as.rspec <- function(object, whichwl = NULL, interp = FALSE) {

if (is.matrix(object)) {
  name <- colnames(object)
} else
if (is.data.frame(object)) {
  name <- names(object)
} else {
  stop('object must be a data frame or matrix')
}

# try to automatically find wavelength column
# ind <- sapply(1:ncol(object), function(x) {sd(diff(object[,x]))})
ind <- apply(object, 2, function(x){cor(x, 1:nrow(object))})  # for increasing 
# wavelengths, expect a perfect correlation between lambda values and column 
# indices

if (!is.null(whichwl)){
      wl_index <- whichwl
      wl <- object[, wl_index]
      object <- object[, -wl_index]
      name <- name[-wl_index]
  } else if (any(ind > 0.99)) {
      wl_index <- which(ind > 0.99)[1]
      wl <- object[, wl_index]
      object <- object[, -wl_index]
      name <- name[-wl_index]
      cat('wavelengths found in column', wl_index,'\n')
} else {
  wl <- 1:nrow(object)
  object <- object
  name <- name
  warning('No wavelengths found or whichwl not provided; using arbitrary index values')
}

if (interp==TRUE) {
  wl[which.min(wl)] <- round(min(wl))
  wl[which.max(wl)] <- round(max(wl))
  object <- sapply(1:ncol(object), function(x) approx(x=wl, y=object[,x], 
                   xout = min(wl):max(wl))$y)
  wl <- approx(wl, xout = min(wl):max(wl))$x
}

res <- as.data.frame(cbind(wl, object))

names(res) <- c('wl', name)

class(res) <- c('rspec', 'data.frame')

res

}

#' @rdname as.rspec
#' @return a logical value indicating whether the object is of class \code{rspec}

is.rspec <- function(object){
 inherits(object, "rspec")
}
