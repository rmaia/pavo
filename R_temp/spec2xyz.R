#' Spectrum to rgb color conversion
#'
#' Calculates rgb values from spectra based on human color matching functions
#'
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec},
#' with a column with wavelength data, named 'wl', and the remaining column containing
#' spectra to process.
#' 
#' @return A character vector of class \code{spec2rgb} consisting of hexadecimal color values
#' for passing to further plotting functions.
#' @export
#' @examples \dontrun{
#' data(teal)
#' spec2rgb(teal)
#' # Plot data using estimated perceived color
#' plot(teal, col = spec2rgb(teal), type = 'o')}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @references CIE(1932). Commission Internationale de l'Eclairage Proceedings, 1931. Cambridge: Cambridge University Press.
#' @references Color matching functions obtained from Colour and Vision Research Laboratory 
#' online data respository at \url{http://www.cvrl.org/}.
#' @references \url{http://www.cs.rit.edu/~ncs/color/t_spectr.html}.

spec2xyz <- function(rspecdata) {

wl_index <- which(names(rspecdata)=='wl')
if (length(wl_index > 0)){
  wl <- rspecdata[, wl_index]
  rspecdata <- as.data.frame(rspecdata[, -wl_index])
    } else {
    stop('No wavelengths supplied; no default')
    }

# this should be changed later (interpolate?)

if(min(wl) > 400 | max(wl) < 700)
  stop('wavelength range does not capture the full visible range (400 to 700)')

rspecdata <- rspecdata[which(wl==400):which(wl==700),]
names_rspecdata <- names(rspecdata)
rspecdata <- as.matrix(rspecdata)

sens <- pavo:::ciexyz

P2 <- rspecdata

# P2 <- sapply(1:ncol(rspecdata), function(x) rspecdata[, x] / sum(rspecdata[, x]))  # normalize to sum of 1

# Convolute
X <- apply(sens[, 'x'] * P2, 2, sum)
Y <- apply(sens[, 'y'] * P2, 2, sum)
Z <- apply(sens[, 'z'] * P2, 2, sum)

XYZ <- cbind(X, Y, Z)

XYZ <- t(sapply(1:nrow(XYZ), function(x) XYZ[x, ] / sum(XYZ[x, ])))

setNames(as.data.frame(XYZ), c("X","Y","Z"))

}
