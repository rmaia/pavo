#' Process spectra
#'
#' Applies normalization and/or smoothing to spectra for further analysis or plotting.
#'
#' @inheritParams aggplot
#' @param opt what type of processing options to apply. User can select multiple
#' options by providing a vector. Possibilities are:
#' * `"none"` does not perform any processing (default).
#' * `"smooth"` applies LOESS smoothing to each spectrum using [loess.smooth()].
#' Optimal smoothing parameter can be assessed by using [plotsmooth()].
#' * `"minimum"` subtracts the minimum from each individual spectra.
#' * `"maxmimum"` divides each spectrum by its maximum value.
#' * `"sum"` divides each spectrum by summed values.
#' * `"bin"` bins each spectrum into the specified number of bins. `bins`
#' argument must be set.
#' * `"center"` centers individual spectra by subtracting mean reflectance from
#' all values.
#' @param fixneg how to handle negative values. Possibilities are:
#' * `"none"` does not perform negative value correction (default).
#' * `"zero"` sets all negative values to zero.
#' * `"addmin"` adds the absolute value of the maximally negative values of each
#' spectra to the reflectance at all other wavelengths (setting the minimum
#' value to zero, but scaling other values accordingly).
#' @param span sets the smoothing parameter used by [loess.smooth()].
#' @param bins sets the number of equally sized wavelength bins for `opt = "bin"`.
#'
#' @return A data frame of class `rspec` with the processed data.
#'
#' @export
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @examples
#' data(teal)
#' plot(teal, select = 10)
#'
#' # Smooth data to remove noise
#' teal.sm <- procspec(teal, opt = "smooth", span = 0.25)
#' plot(teal.sm, select = 10)
#'
#' # Normalize to max of unity
#' teal.max <- procspec(teal, opt = c("max"))
#' plot(teal.max, select = 10)
#' @seealso [loess.smooth()], [plotsmooth()]
#'
#' @references Cuthill, I., Bennett, A. T. D., Partridge, J. & Maier, E. 1999.
#'  Plumage reflectance and the objective assessment of avian sexual dichromatism.
#'  The American Naturalist, 153, 183-200.
#' @references Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw,
#'  K.J., eds. Bird Coloration. Volume 1 Mechanisms and measurements. Harvard
#'  University Press, Cambridge, Massachusetts.
#' @references White, T. E., Dalrymple, R. L., Noble D. W. A., O'Hanlon, J. C.,
#' Zurek, D. B., Umbers, K. D. L. 2015. Reproducible research in the study
#' of biological coloration. Animal Behaviour, 106, 51-57.

procspec <- function(rspecdata, opt = c(
                       "none", "smooth", "maximum", "minimum",
                       "bin", "sum", "center"
                     ),
                     fixneg = c("none", "addmin", "zero"),
                     span = 0.25, bins = 20) {
  opt <- match.arg(opt, several.ok = TRUE)

  fixneg <- match.arg(fixneg)

  applied <- "processing options applied:"

  if (any(opt == "none")) {
    opt <- "none" # remove other opt arguments (so they are not called further on, but still allowing for fixneg to work)

    if (fixneg == "none") {
      stop("No processing options selected")
    }
  }

  wl_index <- which(names(rspecdata) == "wl")

  if (length(wl_index > 0)) {
    wl <- rspecdata[, wl_index]
    rspecdata <- rspecdata[-wl_index]
  } else {
    warning("No wavelengths supplied; using arbitrary values")
    rspecdata <- rspecdata
    wl <- seq_len(nrow(rspecdata))
  }

  nam <- names(rspecdata)

  if (any(opt == "smooth")) {
    rspecdata <- apply(rspecdata, 2, function(z) {
      loess.smooth(
        x = wl,
        y = z, span = span, degree = 2,
        family = "gaussian", evaluation = length(wl)
      )$y
    })
    applied <- c(applied, paste("smoothing spectra with a span of", span))
  }

  mins <- apply(rspecdata, 2, min)
  maxs <- apply(rspecdata, 2, max)

  if (fixneg == "addmin") {
    rspecdata <- t(t(rspecdata) + abs(pmin(0, mins)))
    applied <- c(applied, "Negative value correction: added min to all reflectance")
  }

  if (fixneg == "zero") {
    rspecdata[rspecdata < 0] <- 0
    applied <- c(applied, "Negative value correction: converted negative values to zero")
  }

  if (any(opt == "minimum")) {
    rspecdata <- t(t(rspecdata) - mins)
    applied <- c(applied, "Scaling spectra to a minimum value of zero")
  }

  if (any(opt == "maximum")) {
    rspecdata <- t(t(rspecdata) / maxs)
    applied <- c(applied, "Scaling spectra to a maximum value of 1")
  }

  if (any(opt == "sum")) {
    rspecdata <- t(t(rspecdata) / colSums(rspecdata))
    applied <- c(applied, "Scaling spectra to a total area of 1")
  }

  if (any(opt == "center")) {
    rspecdata <- t(t(rspecdata) - colMeans(rspecdata))
    applied <- c(applied, "Centering spectra to a mean of zero")
  }

  # Calculate medians according to # of bins specified for use in PCA
  # Method follows Cuthill et al. (1999)
  if (any(opt == "bin")) {
    bw <- floor(length(wl) / (bins - 1))
    wl_bin <- wl_bin <- seq(min(wl), by = bw, length.out = bins)
    rspecdata <- by(rspecdata, findInterval(wl, wl_bin, rightmost.closed = TRUE, left.open = TRUE), function(x) apply(x, 2, median))
    rspecdata <- do.call(rbind, rspecdata)
    rspecdata <- cbind(wl_bin, rspecdata)
    applied <- c(applied, paste("binned spectra to ", bw, "-nm intervals\n", sep = ""))
  } else {
    rspecdata <- cbind(wl, rspecdata)
  }

  rspecdata <- as.data.frame(rspecdata)
  names(rspecdata) <- c("wl", nam)
  class(rspecdata) <- c("rspec", "data.frame")

  applied <- paste(applied, collapse = "\n")
  message(applied)

  rspecdata
}
