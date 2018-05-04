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
#' @note if more than one value is given to \code{subset}, any spectra that matches \emph{either}
#' condition will be included. It's a union, not an intersect.
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
  
  # This could be simplified a lot by using grepl instead of grep, removing 
  # which and using boolean operations. But at the moment, grep supports an
  # additional argument `invert` that grepl doesn't. Because some scripts maybe
  # already depends on it, we can't really change it.
  
  wl_index <- which(names(x)=="wl")
  
  if (is.logical(subset)) {
    if (length(subset)!=ncol(x)){
      warning("Subset doesn't match length of spectral data")
    }
    subsample <- which(subset)
  }
  else {
    subsample <- grep(pattern=paste(subset, collapse="|"), x = colnames(x), ...)
  }
  if (length(subsample)==0) {
    warning("Subset condition not found")
  }

  # We don't drop the "wl" column if it exists, no matter what subset says.
  res = x[, c(wl_index, subsample)]

  class(res) <- c("rspec", "data.frame")
  
  return(res)
}

#' @export
#' @rdname subset.rspec
#'
subset.colspace <- function (x, subset, ...) {

  if (!is.logical(subset)) {
    subset <- grep(paste(subset, collapse='|'), row.names(x), ...)
  }
  
  res <- x[subset, ]
  
  if (nrow(res)==0) {
    warning("Subset condition not found")
  }

  class(res) <- c("colspace", "data.frame")
  
  return(res)
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
