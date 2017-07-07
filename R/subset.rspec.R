#' Subset rspec, vismodel, and colspace objects
#'
#' Subsets various object types based on a given vector or grep partial matching of data names.
#'
#' @param x (required) an object of class \code{rspec}, \code{vismodel}, or \code{colspace},
#' containing spectra, visual model output or colorspace data to subset.
#' @param ... additional attributes passed to \code{grep}. Ignored if \code{subset} is logical.
#' @param subset a string used for partial matching of observations.
#' @return a subsetted object of the same class as the input object.
#'
#' @export
#'
#' @examples \dontrun{
#' data(sicalis)
#' vis.sicalis <- vismodel(sicalis)
#' tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')
#' 
#' # Subset all 'crown' patches (C in file names)
#' head(subset(sicalis, "C"))
#' subset(vis.sicalis, "C")
#' subset(tcs.sicalis, "C")[, 1:5]
#' subset(sicalis, c("B","C"))
#' subset(sicalis, "T", invert=TRUE)
#' }
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

subset.rspec <- function (x, subset, ...) {
  
  # remove 'wl' column if present
  wl_index <- which(names(x)=='wl')
  if (length(wl_index)==1) {
    wl <- x[, wl_index]
    x <- x[, -wl_index, drop=FALSE]
  }
  
  if (is.logical(subset)) {
    # test whether 'wl' is in subset condition
    # gets from function call for subset
    subsample <- substitute(subset)
    if ('wl' %in% eval(subsample[[2]])) {
      subsample[[2]] <- eval(subsample[[2]])[-wl_index]
    }
    subsample <- which(eval(subsample))
    # check that subset same length as number of spectra
    if (length(subsample)!=ncol(x)){
      warning("Subset doesn't match length of spectral data")
    }
  } else {
    subsample <- grep(pattern=paste(subset, collapse='|'), x=names(x), ...)
  }
  if (length(subsample)==0) {
    warning("Subset condition not found")
  }
  res <- cbind(wl, x[subsample])
  class(res) <- c("rspec", "data.frame")
  res
}

#' @export
#' @rdname subset.rspec
#'
subset.colspace <- function (x, subset, ...) {
  # if (!is.logical(subset)) 
  #   stop("'subset' must be logical")
  if (is.logical(subset)) {
    subsample <- subset
    res <- x[which(subsample), ]
  } else {
      subsample <- grep(paste(subset, collapse='|'), row.names(x), ...)
      res <- x[subsample, ]
    }
  if (length(subsample)==0) {
    warning("Subset condition not found")
  }
  class(res) <- c("colspace", "data.frame")
  res
}

#' @export
#' @rdname subset.rspec
#'
subset.vismodel <- function (x, subset, ...) {
  if (is.logical(subset)) {
    subsample <- subset
    res <- x[which(subsample), ]
  } else {
      subsample <- grep(paste(subset, collapse='|'), row.names(x), ...)
      res <- x[subsample, ]
    }
  # attr <- attributes(x)
  class(res) <- c("vismodel", "data.frame")
  res
}
