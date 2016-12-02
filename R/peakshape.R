#' Peak shape descriptors
#'
#' Calculates height, location and width of peak at the reflectance midpoint (FWHM).
#' Note: bounds should be set wide enough to incorporate all minima in spectra. Smoothing
#' spectra using \code{\link{procspec}} is also recommended.
#' @param rspecdata (required) a data frame, possibly an object of class \code{rspec},
#' with a column with wavelength data, named 'wl', and the remaining column containing
#' spectra to process.
#' @param select specification of which spectra to plot. Can be a numeric vector or 
#' factor (e.g., \code{sex=='male'})
#' @param lim a vector specifying the wavelength range to analyze
#' @param plot logical. Should plots indicating calculated parameters be returned? 
#' (Defaults to \code{TRUE})
#' @param ask logical, specifies whether user input needed to plot multiple plots
#' when number of spectra to analyze is greater than 1 (defaults to \code{FALSE})
#' @param ... additional arguments to be passed to plot
#' @return a data frame containing column names (id); peak height (max value, B3), location (hue, H1) and full width
#' at half maximum (FWHM), as well as half widths on left (HWHM.l) and right side of peak (HWHM.r). Incl.min column
#' indicates whether user-defined bounds incorporate the actual minima of the spectra.
#' Function will return a warning if not.
#' @seealso \code{\link{procspec}}
#' @export
#' @examples \dontrun{
#' data(teal)
#' peakshape(teal, select = 3)
#' peakshape(teal, select = 10)
#' # Use wavelength bounds to narrow in on peak of interest
#' peakshape(teal, select = 10, lim=c(400, 550))}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}, Rafael Maia \email{rm72@@zips.uakron.edu}

peakshape <- function(rspecdata, select = NULL, lim = c(300, 700), 
                      plot = TRUE, ask = FALSE, ...) {

old.par <- par(no.readonly = TRUE)  # all par settings that could be set

par(ask = ask)

# if (length(select) > 1)
#   par(mfrow=c(2,2)) else
#   par(mfrow=c(1,1))

nms <- names(rspecdata)

wl_index <- which(names(rspecdata)=='wl')
if (length(wl_index) > 0) {
  haswl <- TRUE
  wl <- rspecdata[, wl_index]
} else {
  haswl <- FALSE
  wl <- 1:nrow(rspecdata)
  warning('No wavelengths provided; using arbitrary index values')
}

# subset based on indexing vector
if (is.logical(select))
  select <- which(select=='TRUE')
if (is.null(select)&haswl==TRUE)
  select <- (1:ncol(rspecdata))[-wl_index]
if (is.null(select)&haswl==FALSE)
  select <- 1:ncol(rspecdata)

rspecdata <- as.data.frame(rspecdata[, select])


wlrange <- lim[1]:lim[2]

if (ncol(rspecdata)==1) {
  rspecdata2 <- rspecdata[(which(wl==lim[1])):(which(wl==lim[2])), ]  # working wl range
  Yi <- max(rspecdata2)  # max refls
  Yj <- min(rspecdata2)  # min refls
  Yk <- min(rspecdata)  # min refls, whole spectrum
  Xi <- which(rspecdata2==Yi)  # lambda_max index
  fsthalf <- rspecdata2[1:Xi]
  sndhalf <- rspecdata2[Xi:length(rspecdata2)]
  halfmax <- (Yi + Yj) / 2  # reflectance midpoint
  fstHM <- which.min(abs(fsthalf - halfmax))
  sndHM <- which.min(abs(sndhalf - halfmax))
} else {
  rspecdata2 <- rspecdata[(which(wl==lim[1])):(which(wl==lim[2])), ]  # working wl range
  Yi <- apply(rspecdata2, 2, max)  # max refls
  Yj <- apply(rspecdata2, 2, min)  # min refls
  Yk <- apply(rspecdata, 2, min)  # min refls, whole spectrum
  Xi <- sapply(1:ncol(rspecdata2), function(x) which(rspecdata2[, x]==Yi[x]))  # lambda_max index
  fsthalf <- lapply(1:ncol(rspecdata2), function(x) rspecdata2[1:Xi[x], x])
  sndhalf <- lapply(1:ncol(rspecdata2), function(x) rspecdata2[Xi[x]:nrow(rspecdata2), x])
  halfmax <- (Yi + Yj) / 2  # reflectance midpoint
  fstHM <- sapply(1:length(fsthalf), function(x) which.min(abs(fsthalf[[x]]-halfmax[x])))
  sndHM <- sapply(1:length(sndhalf), function(x) which.min(abs(sndhalf[[x]]-halfmax[x])))

}


if (any(Yj>Yk)) {
warning(paste('Please fix lim in spectra with incl.min marked "No" to 
              incorporate all minima in spectral curves'))
}

Xa <- wlrange[fstHM]
Xb <- wlrange[Xi+sndHM]
hue <- wlrange[Xi]

if (plot==TRUE) {
  for (i in seq_along(select)) {
    plot(rspecdata[, i]~wl, type = 'l', xlab = "Wavelength (nm)", 
         ylab = "Reflectance (%)", main = nms[select[i]], ...)
    abline(v = hue[i], col = "red")
    abline(h = halfmax[i], col = "red")
    abline(v = Xa[i], col = "red", lty = 2)
    abline(v = Xb[i], col = "red", lty = 2)
    abline(v = lim, col = "lightgrey")
  }
}

out <- data.frame(id = nms[select], B3 = as.numeric(Yi), H1 = hue, 
                  FWHM = Xb - Xa, HWHM.l = hue - Xa, HWHM.r = Xb - hue, 
                  incl.min = c("Yes", "No")[as.numeric(Yj>Yk)+1])

# row.names(out) <- nms[select]

par(old.par)

out

}
