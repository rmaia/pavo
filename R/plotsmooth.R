#' Plot loess smoothed curves
#'
#' Plots spectral curves with various levels of loess smoothing to help decide which
#' loess parameters are best for subsequently smoothing the data (e.g. via [procspec()]).
#'
#' @inheritParams aggplot
#' @param minsmooth the minimum f value of the loess function to visualize (defaults to `0.05`).
#' @param maxsmooth the maximum f value of the loess function to visualize (defaults to `0.20`).
#' @param curves the number of curves to display on the same plot (defaults to `5`).
#' @param specnum the number of spectral curves, from the data frame, to visualize (defaults to `ALL`).
#' @param ask logical. if `TRUE`, asks for user input before changing plot pages
#'
#' @return Series of plot with curves processed with varying level of loess smoothing
#'
#' @importFrom graphics legend lines mtext par title
#' @importFrom stats loess.smooth
#'
#' @export
#'
#' @seealso [procspec()]
#'
#' @examples
#' # Load reflectance spectra
#' data(sicalis)
#'
#' # Visualise the spectral reflectance curves across a range of smoothing levels
#' plotsmooth(sicalis, minsmooth = 0.05, maxsmooth = 0.1, curves = 7, specnum = 6)
#'
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}

plotsmooth <- function(rspecdata, minsmooth = 0.05, maxsmooth = 0.20,
                       curves = 5, specnum = "ALL", ask = TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  curves <- curves + 1

  wl <- isolate_wl(rspecdata, keep = "wl")
  rspecdata2 <- isolate_wl(rspecdata, keep = "spec")

  # Remove spectra according to specnum
  if (specnum != "ALL") {
    rspecdata2 <- rspecdata2[, seq_len(specnum)]
  }

  titlenames <- names(rspecdata2)

  nplots <- ncol(rspecdata2)

  span_values <- seq(minsmooth, maxsmooth, length.out = curves - 1)

  legnames <- sprintf("span = %.4s", span_values)
  legnames <- rev(c("raw", legnames))

  # Sets plot parameters based on the number of curves on the plots
  par(mfrow = c(3, 4), ask = ask)
  numplots <- 12

  if (curves > 4) {
    par(mfrow = c(2, 3))
    numplots <- 6
  }
  if (curves > 7) {
    par(mfrow = c(2, 2))
    numplots <- 4
  }
  if (curves > 9) {
    par(mfrow = c(1, 2))
    numplots <- 2
  }
  if (curves > 12) {
    par(mfrow = c(1, 1))
    numplots <- 1
  }

  if (nplots == 1) {
    par(mfrow = c(1, 1))
    numplots <- 1
    par(ask = FALSE)
  }

  par(mar = c(2, 2, 2, 2), oma = c(3, 3, 0, 0))

  col_list <- c(
    "#000000", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#ffdd33", "#A65628", "#F781BF"
  )
  cols <- col_list[seq_len(curves)]

  # Creates the smooth data matrix
  for (i in seq_len(nplots)) {
    yaxismax <- max(rspecdata2[, i]) + (curves - 1) * 5

    plot(wl, rspecdata2[, i], cex = 0.1, ylim = c(0, yaxismax + 5), type = "l")
    legend(wl[1] - 20, yaxismax + 6, legend = legnames, text.col = rev(cols), cex = 0.7, bty = "n", xjust = 0)
    title(titlenames[i])

    for (j in seq_len(curves - 1)) {
      lines(wl,
        loess.smooth(wl, rspecdata2[, i],
          span = span_values[j],
          evaluation = length(wl), degree = 2, family = "gaussian"
        )$y + (j * 5),
        col = cols[j + 1]
      )
    }

    if (i %% numplots == 0) {
      mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
      mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)
    }
  }

  if (i %% numplots != 0) {
    mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
    mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)
  }
}
