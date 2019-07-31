isolate_wl <- function(rspecdata, keep = c("wl", "spec")) {

  keep <- match.arg(keep)

  is_wl <- colnames(rspecdata) == "wl"

  if (keep == "wl") {
    if (any(is_wl)) {
      return(rspecdata[, is_wl])
    } else {
      warning("wl column missing from input rspec data. Using arbritrary ",
              "values based on object length.", call. = FALSE)
      return(seq_len(nrow(rspecdata)))
    }
  } else {
    return(rspecdata[, !is_wl, drop = FALSE])
  }
}
