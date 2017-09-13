#' Spectrum to rgb color conversion
#'
#' Calculates rgb values from spectra based on human color matching functions.
#'
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec},
#' with a column with wavelength data, named 'wl', and the remaining column containing
#' spectra to process.
#' @param alpha alpha value to use for colors (defaults to 1, opaque).
#' 
#' @return A character vector of class \code{spec2rgb} consisting of hexadecimal color values
#' for passing to further plotting functions.
#' 
#' @export
#' 
#' @examples \dontrun{
#' data(teal)
#' spec2rgb(teal)
#' 
#' # Plot data using estimated perceived color
#' plot(teal, col = spec2rgb(teal), type = 'o')}
#' 
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' 
#' @references CIE(1932). Commission Internationale de l'Eclairage Proceedings, 1931. Cambridge: Cambridge University Press.
#' @references Color matching functions obtained from Colour and Vision Research Laboratory 
#' online data respository at \url{http://www.cvrl.org/}.
#' @references \url{http://www.cs.rit.edu/~ncs/color/t_spectr.html}.

spec2rgb <- function(rspecdata, alpha = 1){

wl_index <- which(names(rspecdata)=='wl')
if (length(wl_index > 0)){
  wl <- rspecdata[, wl_index]
  rspecdata <- as.data.frame(rspecdata[, -wl_index, drop=FALSE])
    } else {
    stop('No wavelengths supplied; no default')
    }

# this should be changed later (interpolate?)

if(min(wl) > 400 | max(wl) < 700)
  stop('wavelength range does not capture the full visible range (400 to 700)')

rspecdata <- rspecdata[which(wl==400):which(wl==700),]
names_rspecdata <- names(rspecdata)
rspecdata <- as.matrix(rspecdata)

# TEMP: cie2 or cie10?
# sens <- ciexyz[,1:4] #cie2
sens <- ciexyz[,c(1,5:7)] #cie10

# TEMP: removing wavelengths 390:400
# TO DO: check if rspecdata starts at 400 or <400 and change this accordingly

sens <- sens[which(sens$wl==400):which(sens$wl==700),]

# P2 <- sapply(1:ncol(rspecdata), function(x) rspecdata[, x] / sum(rspecdata[, x]))  # normalize to sum of 1
# P2 <- rspecdata
P2 <- rspecdata / 100  # scale to proportion of incident light

# Convolute
X <- apply(sens[, grep('x', names(sens))] * P2, 2, sum)
Y <- apply(sens[, grep('y', names(sens))] * P2, 2, sum)
Z <- apply(sens[, grep('z', names(sens))] * P2, 2, sum)

# Scale by y(lambda)
N <- sum(sens[, grep('y', names(sens))])  # normalization factor
XYZ <- cbind(X, Y, Z) / N

# transfer matrices for converting XYZ to RGB
# source: http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
# sRGB
xyzmat <- rbind(c(3.240479, -1.537150, -0.498535),
                c(-0.969256, 1.875992, 0.041556),
                c(0.055648, -0.204043, 1.057311))
# CIE RGB
# xyzmat <- rbind(c(2.3706743, -0.9000405, -0.4706338),
#                 c(-0.5138850, 1.4253036, 0.0885814),
#                 c(0.0052982, -0.0146949, 1.0093968))

# xyz <- t(sapply(1:nrow(XYZ), function(x) XYZ[x, ] / sum(XYZ[x, ])))

# rgb1 <- t(sapply(1:nrow(XYZ), function(x) xyzmat%*%as.matrix(XYZ[x, ])))
rgb1 <- t(sapply(1:nrow(XYZ), function(x) {xyzmat %*% XYZ[x, ]}))

# sRGB companding (e.g., see http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html)
rgb1 <- sapply(1:ncol(rgb1), function(x) {
  ifelse(rgb1[,x] <= 0.0031308, 12.92*rgb1[,x], 1.055*rgb1[,x]^(1/2.4) - 0.055)
  })

# clip RGB values outside {0-1}
rgb1[rgb1 < 0] <- 0
rgb1[rgb1 > 1] <- 1

colrs <- rgb(red=rgb1[, 1], green=rgb1[, 2], blue=rgb1[, 3], alpha=alpha)

#class(colrs) <- c('spec2rgb', 'character')

names(colrs) <- names_rspecdata

colrs

}
