#' Convert data to an rspec object
#'
#' Converts data frames or matrices containing spectral data to `rspec`
#' object
#'
#' @param object (required) a data frame or matrix containing spectra to
#'   process.
#' @param whichwl a numeric or character vector specifying which column contains
#'   wavelengths. If `NULL` (default), function searches for column containing
#'   equally spaced numbers and sets it as wavelengths "wl". If no wavelengths
#'   are found or `whichwl` is not given, returns arbitrary index values.
#' @param interp whether to interpolate wavelengths in 1-nm bins (defaults to
#'   `TRUE`). It is rarely recommended to turn off this option, as
#'   uninterpolated spectra are incompatible with some downstream analyses,
#'   including notably colour vision models.
#' @param lim vector specifying wavelength range to interpolate over (e.g.
#'   `c(300, 700)`).
#' @param exceed.range logical. Should data be interpolated to the limits
#'   specified by `lim` if `lim` exceeds the range of the actual data? Useful,
#'   and relatively safe, when the data range falls slightly within `lim` (e.g.
#'   300.1 - 699 nm), but will produce spurious results if `lim` far exceeds the
#'   range of input data. Defaults to `TRUE`.
#'
#' @return an object of class `rspec` for use in further `pavo` functions
#'
#' @importFrom stats approx cor
#'
#' @export as.rspec is.rspec
#'
#' @examples
#'
#' # Generate some fake reflectance data
#' fakedat <- data.frame(wl = 300:700, refl1 = rnorm(401), refl2 = rnorm(401))
#' head(fakedat)
#'
#' # Determine if is rspec object
#' is.rspec(fakedat)
#'
#' # Convert to rspec object
#' fakedat2 <- as.rspec(fakedat)
#' is.rspec(fakedat2)
#' head(fakedat2)
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

as.rspec <- function(object, whichwl = NULL,
                     interp = TRUE, lim = NULL, exceed.range = TRUE) {

  # tibble dodge
  if (inherits(object, "tbl_df")) {
    object <- data.frame(object, check.names = FALSE)
  }

  if (is.matrix(object) || is.data.frame(object)) {
    name <- colnames(object)
  } else {
    stop("object must be a data frame or matrix", call. = FALSE)
  }

  if (!all(vapply(seq_len(ncol(object)), function(j) is.numeric(object[, j]), logical(1)))) {
    stop("all columns must contain numeric data", call. = FALSE)
  }

  if (anyNA(object)) {
    message(
      "The spectral data contain ", sum(is.na(object)),
      " NA's(s), which should be reviewed closely."
    )
  }

  # How to handle wavelength column.
  # Possible conditions for wavelength column:
  #            |  specified   | not specified
  # -----------------------------------------
  # given      |      1       |     3
  # not given  |      2       |     4
  # Case 1: wl | col1 | col2... whichwl=... --> use whichwl
  # Case 2:      col1 | col2...; lim=c(300, 700) --> use lim[1]:lim[2]
  # Case 3: wl | col1 | col2... (no whichwl, lim) --> use correlation find
  # Case 4:      col1 | col2... --> use arbitrary numbering


  if (!is.null(whichwl)) {
    if (is.numeric(whichwl)) {
      wl_index <- whichwl
    } else if (is.character(whichwl)) {
      wl_index <- which(colnames(object) == whichwl)
    }
    wl <- object[, wl_index]
    object <- object[, -wl_index, drop = FALSE]
    name <- name[-wl_index]
  } else {
    # try to automatically find wavelength column. for increasing wavelengths,
    # expect a near perfect correlation between lambda values and row indices
    ind <- apply(object, 2, cor, seq_len(nrow(object)))

    if (any(ind > 0.999)) {
      wl_index <- which(ind > 0.999)[1]
      wl <- object[, wl_index]
      object <- object[, -wl_index, drop = FALSE]
      name <- name[-wl_index]
      message("wavelengths found in column ", wl_index)
    } else if (!is.null(lim)) {
      wl <- seq(lim[1], lim[2], length.out = nrow(object))
      warning(
        "No wavelengths contained in dataset, using user-specified range.\n",
        "Check output carefully!",
        call. = FALSE
      )
    } else {
      wl <- seq_len(nrow(object))
      warning(
        "No wavelengths found or whichwl not provided; ",
        "using arbitrary index values.",
        call. = FALSE
      )
    }
  }

  l1.dat <- floor(wl[which.min(wl)]) # lower wavelength limit of given data
  l2.dat <- floor(wl[which.max(wl)]) # upper wavelength limit of given data

  # Get data limits
  if (is.null(lim)) {
    l1 <- l1.dat
    l2 <- l2.dat
  } else {
    l1 <- lim[1]
    l2 <- lim[2]
    if (l1.dat > lim[1] || l2.dat < lim[2]) {
      if (exceed.range) {
        warning(
          "Interpolating beyond the range of actual data.\n",
          "Check 'lim' and `exceed.range` arguments to confirm this is the desired behaviour.",
          call. = FALSE
        )
      }
    }
  }

  # Interpolation & data-trimming
  rule <- ifelse(exceed.range, 2, 1)

  if (interp) {
    object <- apply(object, 2, function(col) {
      approx(x = wl, y = col, xout = l1:l2, rule = rule)$y
    })
    wl <- seq(l1, l2)
  } else {
    object <- object[wl >= l1 & wl <= l2, ]
    wl <- wl[wl >= l1 & wl <= l2]
  }

  # We need to convert object as a df before putting back the wl, otherwise
  # altrep INTSXP wl will be converted to REALSXP
  object <- as.data.frame(object)
  res <- cbind(wl, object)

  colnames(res) <- c("wl", name)

  wl_index <- which(colnames(res) == "wl")

  if (length(wl_index) > 1) {
    warning("Multiple columns named 'wl', check column names", call. = FALSE)
    colnames(res)[wl_index] <- c("wl", paste0("wl.", wl_index[-1] - 1))
  }

  # Negative value check
  if (any(res < 0, na.rm = TRUE)) {
    message(
      "The spectral data contain ", sum(res < 0, na.rm = TRUE),
      " negative value(s),\n",
      " which may produce unexpected results if used in models.\n",
      "Consider using procspec() to correct them."
    )
  }

  class(res) <- c("rspec", "data.frame")

  return(res)
}

#' @rdname as.rspec
#' @return a logical value indicating whether the object is of class `rspec`

is.rspec <- function(object) {
  inherits(object, "rspec")
}
