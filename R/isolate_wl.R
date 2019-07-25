isolate_wl <- function(rspecdata, keep = c("wl", "spec")) {

  is_wl <- colnames(rspecdata) == "wl"

  if (keep == "wl") {
    if (any(is_wl)) {
      return(rspecdata[, is_wl])
    } else {
      warning("wl column missing from input rspec data. Defaulting to 300-700",
              "nm range.", call. = FALSE)
      return(300:700)
    }
  } else {
    return(rspecdata[, !is_wl, drop = FALSE])
  }
}
