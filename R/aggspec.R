#' Aggregate reflectance spectra
#'
#' Combines spectra (by taking the average, for example) according to an index
#' or a vector of identities.
#'
#' @inheritParams aggplot
#' @param FUN the function to be applied to the groups of spectra. (defaults to
#'   [mean()])
#' @param trim logical. if `TRUE` (default), the function will try to identify
#'   and remove numbers at the end of the names of the columns in the new rspec
#'   object.
#'
#' @return A data frame of class `rspec` containing the spectra after applying
#'   the aggregating function.
#'
#' @export
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @examples
#' data(teal)
#'
#' # Average every two spectra
#' teal.sset1 <- aggspec(teal, by = 2)
#' plot(teal.sset1)
#'
#' # Create factor and average spectra by levels 'a' and 'b'
#' ind <- rep(c("a", "b"), times = 6)
#' teal.sset2 <- aggspec(teal, by = ind)
#'
#' plot(teal.sset2)
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds)
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.

aggspec <- function(rspecdata, by = NULL, FUN = mean, trim = TRUE) {

  wl <- isolate_wl(rspecdata, keep = "wl")
  y <- isolate_wl(rspecdata, keep = "spec")

  if (is.null(by)) {
    dat <- apply(y, 1, FUN)
    res <- data.frame(cbind(wl = wl, dat))
    class(res) <- c("rspec", "data.frame")
    return(res)
  }

  if (is.numeric(by)) {
    if (ncol(y) %% by != 0) {
      stop("by not a multiple of number of spectra")
    }
  }

  # Check if the by argument has a 'wl' entry (e.g. if names were obtained
  # through regex conditions on the original spec names) and remove it

  if ("wl" %in% by) {
    by <- by[by != "wl"]
  }

  # Handle when 'by' is a list of factors
  if (is.list(by)) {
    wl_id <- vapply(by, function(x) which(x == "wl"), numeric(1)) # extract wl columns
    # remove 'wl' column from each vector in list
    if (any(vapply(wl_id, length, numeric(1)) != 0)) {
      id <- which(vapply(wl_id, length, numeric(1)) != 0)
      by[id] <- lapply(by[id], "[", -unlist(wl_id)[id])
    }
    # check that wl column is the same for all vectors
    if (length(unique(wl_id)) == 1) {
      by <- do.call("paste", c(by, sep = "."))
    } else {
      stop("mismatch in column names of input vectors")
    }
  }

  # retain original 'by' values
  by0 <- by

  # Allow for means of every "by" data, if "by" is a single number
  # i.e. if by=3, average every 3 consecutive data of "data"
  if (length(by) == 1) {
    by0 <- names(y)[seq(1, length(names(y)), by = by)]
    by <- rep(seq_len(length(y) / by), each = by)
  }

  # check: does data have the same number of columns as the by vector?

  if (dim(y)[2] != length(by)) {
    stop(
      dQuote(deparse(substitute(by))),
      " is not of same length as columns in ",
      dQuote(deparse(substitute(data)))
    )
  }

  # Add ability to aggregate based on multiple vectors (given a list as input)

  by <- factor(by) # is this necessary?

  # Convert to data.frame now as to retain ALTREP wl when using cbind() later
  dat <- data.frame(sapply(unique(by), function(z) {
    apply(y[which(by == z)], 1, FUN)
  }))

  colnames(dat) <- unique(by0)

  if (trim) {
    colnames(dat) <- gsub("[._-][0-9]*$", "", colnames(dat))
  }

  res <- cbind(wl = wl, dat)

  class(res) <- c("rspec", "data.frame")

  res

}
