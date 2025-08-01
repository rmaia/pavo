check_data_for_colspace <- function(
  vismodeldata,
  expected_colnames,
  force_relative
) {

  expected_conenumb <- length(expected_colnames)
  chromaticity <- switch(
    as.character(expected_conenumb),
    `2` = "dichromatic",
    `3` = "trichromatic",
    `4` = "tetrachromatic"
  )

  if (is.vismodel(vismodeldata)) {
    actual_conenumb <- attr(vismodeldata, "conenumb")
    msg <- "Input data is a vismodel object "
  } else {
    actual_conenumb <- ncol(vismodeldata)
    msg <- "Input data is not a vismodel object "
  }

  if (actual_conenumb < expected_conenumb) {
    if (is.vismodel(vismodeldata)) {
      msg <- c(
        msg,
        sprintf("and is not %s (too few receptor columns).", chromaticity)
      )
    } else {
      msg <- c(
        msg,
        sprintf("and has fewer than %i columns.", expected_conenumb)
      )
    }
    stop(msg, call. = FALSE)
  }

  if (actual_conenumb > expected_conenumb) {
    if (is.vismodel(vismodeldata)) {
      msg <- c(
        msg,
        sprintf("and is not %s. Extra receptors will be dropped.", chromaticity)
      )
    } else {
      msg <- c(
        msg,
        sprintf("and has more than %i columns. ", expected_conenumb),
        "Extra columns will be dropped."
      )
    }
    warning(msg, call. = FALSE)
  }

  if (!is.vismodel(vismodeldata) && expected_conenumb == actual_conenumb) {
    warning(
      "Treating columns as quantum catch for ",
      toString(expected_colnames), ".",
      call. = FALSE
    )
  }

  if (all(expected_colnames %in% colnames(vismodeldata))) {
    dat <- vismodeldata[, expected_colnames]
  } else {
    warning(
      sprintf(
        "Could not find columns named %s using first %i columns instead.",
        toString(expected_colnames),
        expected_conenumb
      ),
      call. = FALSE
    )
    dat <- vismodeldata[, seq_len(expected_conenumb)]
    colnames(dat) <- expected_colnames
  }

  if (is.vismodel(vismodeldata)) {
    is_relative <- attr(vismodeldata, "relative")
  } else {
    is_relative <- isTRUE(all.equal(rowSums(dat), rep_len(1, nrow(dat)), tolerance = 1e-4))
  }

  if (force_relative && !is_relative) {
    warning(
      "Quantum catch are not relative, and have been transformed.",
      call. = FALSE
    )
    dat <- dat / rowSums(dat)
    attr(dat, "relative") <- TRUE
  }

  return(dat)
}
