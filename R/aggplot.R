#' Plot aggregated reflectance spectra
#'
#' Combines and plots spectra (by taking the average and the standard deviation,
#' for example) according to an index or a vector of identities.
#'
#' @param rspecdata (required) a data frame, possibly of class `rspec`, which
#'   contains a column containing a wavelength range, named 'wl', and spectra
#'   data in remaining columns.
#' @param by (required) either a single value specifying the range of spectra
#'   within the data frame to be combined (for example, `by` = 3 indicates the
#'   function will be applied to groups of 3 consecutive columns in the spectra
#'   data frame); a vector containing identifications for the columns in the
#'   spectra data frame (in which case the function will be applied to each
#'   group of spectra sharing the same identification); or a list of vectors,
#'   e.g., `by = list(sex, species)`.
#' @param FUN.center the function to be applied to the groups of spectra,
#'   calculating a measure of central tendency (defaults to [base::mean()]).
#' @param FUN.error the function to be applied to the groups of spectra,
#'   calculating a measure of variation (defaults to [stats::sd()]).
#' @param lcol colour of plotted lines indicating central tendency.
#' @param shadecol colour of shaded areas indicating variance measure.
#' @param alpha transparency of the shaded areas.
#' @param legend automatically add a legend.
#' @param ... additional graphical parameters to be passed to plot.
#'
#' @return Plot containing the lines and shaded areas of the groups of spectra.
#'
#' @export
#'
#' @examples
#'
#' # Load reflectance data
#' data(sicalis)
#'
#' # Create grouping variable based on spec names
#' bysic <- gsub("^ind[0-9].", "", names(sicalis)[-1])
#'
#' # Plot using various error functions and options
#' aggplot(sicalis, bysic)
#' aggplot(sicalis, bysic, FUN.error = function(x) quantile(x, c(0.0275, 0.975)))
#' aggplot(sicalis, bysic, shadecol = spec2rgb(sicalis), lcol = 1)
#' aggplot(sicalis, bysic, lcol = 1, FUN.error = function(x) sd(x) / sqrt(length(x)))
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu},
#' Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds)
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.

aggplot <- function(rspecdata, by = NULL, FUN.center = mean, FUN.error = sd,
                    lcol = NULL, shadecol = NULL, alpha = 0.2, legend = FALSE, ...) {
  if (isTRUE(by == 1)) {
    stop("Cannot group single spectra (use plot instead)")
  }

  # take aggregated data
  cntplotspecs <- aggspec(rspecdata, by = by, FUN = FUN.center)
  errplotspecs <- aggspec(rspecdata, by = by, FUN = FUN.error)

  if (anyNA(errplotspecs[, -1])) {
    warning("Could not calculate errors. Do any groups have n = 1?",
            call. = FALSE)
  }

  # make wavelength vector
  wl <- isolate_wl(rspecdata, keep = "wl")

  cntplotspecs <- isolate_wl(cntplotspecs, keep = "spec")
  errplotspecs <- isolate_wl(errplotspecs, keep = "spec")

  indexsub <- seq_along(cntplotspecs)

  if (as.integer(dim(errplotspecs)[1] / 2) == as.integer(dim(cntplotspecs)[1]) &&
    as.integer(dim(errplotspecs)[1] %% 2) == 0) {
    polygon_data <- vapply(indexsub, function(x)
      c(
        errplotspecs[seq(dim(errplotspecs)[1]) %% 2 == 1, x],
        rev(errplotspecs[seq(dim(errplotspecs)[1]) %% 2 == 0, x])
      ), numeric(nrow(errplotspecs)))
  } else {
    polygon_data <- vapply(
      indexsub, function(x)
        c(cntplotspecs[, x] + errplotspecs[, x], rev(cntplotspecs[, x] - errplotspecs[, x])),
      numeric(nrow(errplotspecs) * 2)
    )
  }

  polygon_wl <- c(wl, rev(wl))

  # Set sensible plotting defaults
  arg <- list(...)

  if (is.null(arg$xlab)) {
    arg$xlab <- "Wavelength (nm)"
  }
  if (is.null(arg$ylab)) {
    arg$ylab <- "Reflectance (%)"
  }
  if (is.null(arg$xlim)) {
    arg$xlim <- range(wl)
  }
  if (is.null(arg$ylim)) {
    arg$ylim <- range(polygon_data, cntplotspecs, na.rm = TRUE)
  }

  # line width
  if (!is.null(arg$lwd)) {
    lwd <- arg$lwd
  }

  if (is.null(arg$lwd)) {
    lwd <- 1
  }

  if (length(lwd) < ncol(cntplotspecs)) {
    lwd <- rep(lwd, ncol(cntplotspecs))
  }

  # coloring for overlay plot & others
  if (!is.null(arg$lty)) {
    lty <- arg$lty
  }

  if (is.null(arg$lty)) {
    lty <- 1
  }

  if (length(lty) < ncol(cntplotspecs)) {
    lty <- rep(lty, ncol(cntplotspecs))
  }

  if (length(shadecol) < ncol(cntplotspecs)) {
    shadecol <- rep(shadecol, ncol(cntplotspecs))
  }

  if (length(lcol) < ncol(cntplotspecs)) {
    lcol <- rep(lcol, ncol(cntplotspecs))
  }

  col_list <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#FFFF33", "#A65628", "#F781BF"
  )

  col_list <- rep(col_list, length.out = dim(cntplotspecs)[2])

  if (is.null(shadecol)) {
    shadecol <- col_list
  }

  if (is.null(lcol)) {
    lcol <- col_list
  }

  shadecol <- rgb(t(col2rgb(shadecol)) / 255, alpha = alpha)

  # plot polygons first...

  arg$x <- wl
  arg$y <- cntplotspecs[, 1]
  arg$type <- "n"

  arg0 <- arg[names(arg) %in% c(names(formals(plot.default)), names(par()))]
  do.call(plot, arg0)

  arg$type <- NULL
  arg$x <- polygon_wl
  arg$y <- polygon_data[, 1]
  arg$col <- shadecol[1]
  arg$border <- NA

  arg0 <- arg[names(arg) %in% names(formals(polygon))]
  do.call(polygon, arg0)

  if (ncol(cntplotspecs) > 1) {
    for (i in 2:ncol(cntplotspecs)) {
      arg$col <- shadecol[i]
      arg$y <- polygon_data[, i]
      arg0 <- arg[names(arg) %in% c(names(formals(polygon)), names(par()))]
      do.call(polygon, arg0)
    }
  }

  # ...then lines (so they are on top)
  arg$border <- NULL
  arg$col <- lcol[1]
  arg$x <- wl
  arg$y <- cntplotspecs[, 1]
  arg$type <- "l"
  arg$lty <- lty[1]
  arg$lwd <- lwd[1]

  arg0 <- arg[names(arg) %in% c(names(formals(plot.default)), names(par()))]
  do.call(lines, arg0)

  if (ncol(cntplotspecs) > 1) {
    for (i in 2:ncol(cntplotspecs)) {
      arg$y <- cntplotspecs[, i]
      arg$col <- lcol[i]
      arg$lty <- lty[i]
      arg$lwd <- lwd[i]
      arg0 <- arg[names(arg) %in% c(names(formals(plot.default)), names(par()))]
      do.call(lines, arg0)
    }
  }
  if (legend) {
    legend("topleft",
      bty = "n", legend = names(cntplotspecs),
      lty = lty, col = lcol
    )
  }
}
