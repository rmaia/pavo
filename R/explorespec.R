#' Plot spectral curves
#'
#' Plots one or multiple spectral curves in the same graph to rapidly
#' compare groups of spectra.
#'
#' @param by number of spectra to include in each graph (defaults to 1)
#' @param scale defines how the y-axis should be scaled. `"free"`: panels can vary in
#' the range of the y-axis; `"equal"`: all panels have the y-axis with the same range.
#' @param legpos legend position control. Either a vector containing `x` and `y` coordinates
#' or a single keyword from the list: `"bottomright"`, `"bottom"`, `"bottomleft"`,
#' `"left"`, `"topleft"`, `"top"`, `"topright"`, `"right"` and `"center"`.
#' @param ... additional parameters to be passed to plot
#' @inheritParams plotsmooth
#'
#' @return Spectral curve plots
#'
#' @note Number of plots presented per page depends on the number of graphs produced.
#'
#' @export
#'
#' @importFrom graphics legend lines mtext par
#' @importFrom grDevices n2mfrow
#'
#' @examples
#' data(sicalis)
#' explorespec(sicalis, 3)
#' explorespec(sicalis, 3, ylim = c(0, 100), legpos = c(500, 80))
#' @author Pierre-Paul Bitton \email{bittonp@@uwindsor.ca}

explorespec <- function(rspecdata, by = NULL,
                        scale = c("equal", "free"),
                        legpos = "topright", ...) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  wl <- isolate_wl(rspecdata, keep = "wl")
  rspecdata <- isolate_wl(rspecdata, keep = "spec")

  leg2 <- names(rspecdata)

  scale <- match.arg(scale)

  # default by value
  if (is.null(by)) {
    by <- 1
  }

  # check if the by argument has a 'wl' entry (e.g. if names were obtained through
  # regex conditions on the original spec names) and remove it
  if ("wl" %in% by) {
    by <- by[-which(by == "wl")]
  }
  # Allow for means of every "by" data, if "by" is a single number
  # i.e. if by=3, average every 3 consecutive data of "data"
  if (length(by) == 1) {
    by <- rep(seq_len(length(rspecdata) / by), each = by)
  }
  # check: does data have the same number of columns as the by vector?
  # TODO: this is actually alright if you are not plotting by a character vector
  # describing the species, type of patch, etc. (by=3 should work fine here, last
  # plot will only have fewer lines)
  if (dim(rspecdata)[2] != length(by) && !is.integer(by)) {
    stop(
      dQuote(deparse(substitute(by))),
      " is not of same length as columns in ",
      dQuote(deparse(substitute(data)))
    )
  }

  by <- factor(by)

  # number of 'by' groups
  numby <- nlevels(by)

  # setup multi-panel plot parameters
  if (numby <= 4) {
    yaxismult <- c(0, 1.4)
  }
  if (numby > 4) {
    yaxismult <- c(0.9, 1.4)
  }
  if (numby > 7) {
    yaxismult <- c(0.9, 1.8)
  }
  if (numby > 9) {
    yaxismult <- c(0.9, 1.4)
  }
  if (numby > 12) {
    yaxismult <- c(0.9, 1.8)
  }

  if (numby <= 16) {
    par(mfrow = n2mfrow(numby))
  }
  else {
    par(ask = TRUE)
  }


  arg <- list(...)

  arg$x <- wl

  if (is.null(arg$col)) {
    col_list <- viridisLite::viridis(max(table(by)))
  } else {
    col_list <- arg$col
  }

  par(mar = c(2, 2, 1, 1), oma = c(3, 3, 0, 0))

  if (is.null(arg$ylim) && scale == "equal") {
    arg$ylim <- range(rspecdata) * yaxismult
  }

  # TODO: should be able to deal with this if there are NAs in the by vector
  if (anyNA(by)) {
    warning("NA values in by vector. please check.")
  }

  # Do the plotting
  for (i in seq_len(numby)) {
    if (numby == 1) {
      bloc <- data.frame(rspecdata[seq_len(by)])
    } else {
      # THIS IS WHAT I NEED TO FIX
      bloc <- rspecdata[, which(by == levels(by)[i])]
      #       bloc <- rspecdata[, by==unique(by)[i]]
      #       bloc <- rspecdata[,(((i-1)*numby)+1):min(i*numby,dim(rspecdata)[2])]
    }

    # SET OPTIONAL ARGUMENTS
    if (all(is.null(arg$ylim), scale == "free")) {
      arg$ylim <- c(min(bloc), max(bloc)) * yaxismult
    }

    if (is.null(arg$type)) {
      arg$type <- "l"
    }

    if (is.null(arg$ylab)) {
      arg$ylab <- "% Reflectance"
    }

    if (is.null(arg$xlab)) {
      arg$xlab <- "Wavelength (nm)"
    }

    if (length(legpos) > 1) {
      legx <- legpos[1]
      legy <- legpos[2]
    } else {
      legx <- legpos
      legy <- NULL
    }

    leg <- names(bloc)

    if (!is.null(dim(bloc))) {
      legcolor <- rep(col_list, length.out = dim(bloc)[2])
    } else {
      legcolor <- col_list
    }

    if (!is.null(dim(bloc))) {
      arg$y <- bloc[, 1]
      if (is.null(arg$col)) {
        arg$col <- legcolor
      }
      do.call(plot, arg)
    } else {
      arg$col <- legcolor[1]
      arg$y <- bloc
      do.call(plot, arg)
      legend(
        x = legx, y = legy, legend = leg2[i], cex = 0.9, bty = "n",
        text.col = legcolor
      )
    }
    if (numby == 1) {
      legend(
        x = legx, y = legy, legend = leg2[i], cex = 0.9, bty = "n",
        text.col = legcolor
      )
    }
    if (!is.null(dim(bloc))) {
      if (dim(bloc)[2] > 1) {
        for (j in 2:dim(bloc)[2]) {
          arg$y <- bloc[, j]
          arg$col <- legcolor[j]
          do.call(lines, arg)
        }
        legend(x = legx, y = legy, legend = names(bloc), cex = 0.9, bty = "n", text.col = legcolor)
      }
    }
    arg$col <- legcolor[1]
    if (scale == "free") {
      arg$ylim <- NULL
    }
    if (i %% 12 == 0) {
      mtext(arg$xlab, side = 1, outer = TRUE, line = 1)
      mtext(arg$ylab, side = 2, outer = TRUE, line = 1)
    }
  }

  if (i %% 12 != 0) {
    mtext(arg$xlab, side = 1, outer = TRUE, line = 1)
    mtext(arg$ylab, side = 2, outer = TRUE, line = 1)
  }

  # do we need this still?
  if ((dim(rspecdata)[2] / numby) != round((dim(rspecdata)[2] / numby))) {
    warning("by is not a factor of the number of column in rspecdata")
  }
}
