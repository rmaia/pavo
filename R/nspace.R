#' n-chromatic colour space
#'
#' Calculates coordinates and colourimetric variables that represent reflectance
#' spectra in a n-chromatic colour space.
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the
#'   result from [vismodel()] or independently calculated data
#'
#' @importFrom geometry bary2cart
#'
#' @export
#'
#' @references
#' Pike, T.W. Generalised Chromaticity Diagrams for Animals with n-chromatic
#' Colour Vision. J Insect Behav 25, 277–286 (2012).
#' \doi{10.1007/s10905-011-9296-2}
#'
#' @examples
#' fakemantisshrimp <- sensmodel(c(325, 350, 400, 425, 450, 500, 550, 600, 650, 700), beta = FALSE, integrate = FALSE)
#'
#' data(flowers)
#' vis_flowers <- vismodel(flowers, visual = fakemantisshrimp)
#' colsp_flowers <- nspace(vis_flowers)

nspace <- function(vismodeldata) {
  qcatches <- vismodeldata[, colnames(vismodeldata) != "lum"]
  lum <- vismodeldata[, colnames(vismodeldata) == "lum"]
  ncones <- ncol(qcatches)

  # Get relative qcatches
  qcatches <- qcatches / rowSums(qcatches)

  ref <- simplex(ncones)
  bary_coords <- bary2cart(ref, as.matrix(qcatches))
  # Keep names for retrocompatibility
  colnames(bary_coords) <- switch(
    as.character(ncones),
    "2" = "x",
    "3" = c("x", "y"),
    "4" = c("x", "y", "z"),
    paste0("X", seq_len(ncones - 1))
  )

  polar_coords <- cart2sph(bary_coords)
  # Keep names for retrocompatibility
  colnames(polar_coords) <- switch(
    as.character(ncones),
    "2" = "r.vec",
    "3" = c("h.theta", "r.vec"),
    "4" = c("h.theta", "h.phi", "r.vec"),
    colnames(polar_coords)
  )

  res <- data.frame(
    qcatches,
    bary_coords,
    polar_coords,
    lum,
    row.names = rownames(vismodeldata)
  )

  class(res) <- c("colspace", "data.frame")

  # Descriptive attributes (largely preserved from vismodel)
  attr(res, "conenumb") <- ncones
  res <- copy_attributes(
    res,
    vismodeldata,
    which = c(
      "qcatch", "visualsystem.chromatic", "visualsystem.achromatic",
      "illuminant", "background", "relative", "vonkries",
      "data.visualsystem.chromatic", "data.visualsystem.achromatic",
      "data.background"
    )
  )

  maxqcatches <- attr(vismodeldata, "data.maxqcatches")
  if (!is.null(maxqcatches) && ncol(maxqcatches) == ncones) {
    maxqcatches <- maxqcatches / rowSums(maxqcatches)
    attr(res, "data.maxgamut") <- bary2cart(ref, maxqcatches)
  } else {
    attr(res, "data.maxgamut") <- NA
  }

  return(res)
}

simplex <- function(n) {
  res <- matrix(NA_real_, nrow = n, ncol = n - 1)

  for (k in seq_len(n - 1)) {
    res[, k] <- 1/sqrt(k+ k^2) * c(rep_len(-1, k), k, rep_len(0, n - k - 1))
  }

  return(res)
}

cart2sph <- function(mat) {
  # https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates

  n_dims <- ncol(mat)
  n_points <- nrow(mat)

  # Initialize result matrix: n-1 angles + 1 radius
  result <- matrix(
    NA_real_,
    nrow = n_points, ncol = n_dims
  )

  result[, n_dims] <- sqrt(rowSums(mat^2))

  if (n_dims == 1) {
    colnames(result) <- "r.vec"
    return(result)
  }

  for (i in seq_len(n_dims - 1)) {
    sum_of_squares <- rowSums(mat[, (i + 1):n_dims, drop = FALSE]^2)
    result[, i] <- atan2(sqrt(sum_of_squares), mat[, i])
  }

  colnames(result) <- c(paste0("h.phi", seq_len(n_dims - 1)), "r.vec")

  return(result)
}
