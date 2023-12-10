#' Convert JND distances into perceptually-corrected Cartesian coordinates
#'
#' Converts a [coldist()] output into Cartesian coordinates that are
#' perceptually-corrected (i.e. noise-weighted Euclidean distances)
#'
#' @param coldistres (required) the output from a [coldist()] call.
#' @param center logical indicating if the data should be centered on its centroid
#' (defaults to `TRUE`).
#' @param rotate logical indicating if the data should be rotated (defaults to `TRUE`).
#' @param rotcenter should the vectors for rotation be centered in the achromatic
#' center ("achro") or the data centroid ("mean", the default)?
#' @param ref1 the cone to be used as a the first reference. May be `NULL`
#' (for no first rotation in the 3-dimensional case) or must match name
#' in the original data that was used for [coldist()]. Defaults to 'l'.
# " (only used if data has 2 or 3 dimensions)
#' @param ref2 the cone to be used as a the second reference. May be `NULL`
#' (for no first rotation in the 3-dimensional case) or must match name
#' in the original data that was used for [coldist()]. Defaults to 'u'.
#' (only used if data has 3 dimensions).
#' @param axis1 A vector of length 3 composed of 0's and 1's, with
#' 1's representing the axes (x, y, z) to rotate around. Defaults to c(1, 1, 0), such
#' that the rotation aligns with the xy plane (only used if data has 2 or 3 dimensions).
#' Ignored if `ref1` is `NULL` (in 3-dimensional case only)
#' @param axis2 A vector of length 3 composed of 0's and 1's, with
#' 1's representing the axes (x, y, z) to rotate around. Defaults to c(0, 0, 1), such
#' that the rotation aligns with the z axis (only used if data has 3 dimensions).
#' Ignored if `ref2` is `NULL` (in 3-dimensional case only)
#'
#' @examples
#' # Load floral reflectance spectra
#' data(flowers)
#'
#' # Estimate quantum catches visual phenotype of a Blue Tit
#' vis.flowers <- vismodel(flowers, visual = "bluetit")
#'
#' # Estimate noise-weighted colour distances between all flowers
#' cd.flowers <- coldist(vis.flowers)
#'
#' # Convert points to Cartesian coordinates in which Euclidean distances are
#' # noise-weighted.
#' jnd2xyz(cd.flowers)
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#'
#' @importFrom stats dist
#'
#' @export
#'
#' @references Pike, T.W. (2012). Preserving perceptual distances in chromaticity diagrams.
#' Behavioral Ecology, 23, 723-728.
#' @references Maia, R., White, T. E., (2018) Comparing colors using visual models.
#' Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}


jnd2xyz <- function(coldistres, center = TRUE, rotate = TRUE,
                    rotcenter = c("mean", "achro"), ref1 = "l", ref2 = "u",
                    axis1 = c(1, 1, 0), axis2 = c(0, 0, 1)) {
  # Accessory functions
  pos2 <- function(d12, d13, d23) {
    x3 <- d13

    if (d23 >= d12 + d13) {
      x3 <- x3 * -1
    }

    x3
  }

  # Accessory functions
  pos3 <- function(d12, d13, d23) {
    x3 <- (d13^2 - d23^2 + d12^2) / (2 * d12)
    y3 <- rep(0, 2)
    y3sq <- d13^2 - x3^2
    if (y3sq > 0) {
      y3 <- sqrt(y3sq) * c(1, -1)
    }

    matrix(c(rep(x3, 2), y3), ncol = 2, dimnames = list(NULL, c("x", "y")))
  }

  pos4 <- function(d12, d14, d24, d34) {
    x4 <- (d14^2 - d24^2 + d12^2) / (2 * d12)
    y4 <- ((d14^2 - d34^2 + thirdpointxy["y"]^2 + thirdpointxy["x"]^2) / (2 * thirdpointxy["y"])) -
      (x4 * (thirdpointxy["x"] / thirdpointxy["y"]))
    z4 <- rep(0, 2)
    z4sq <- d14^2 - x4^2 - y4^2
    if (z4sq > 0) {
      z4 <- sqrt(z4sq) * c(1, -1)
    }
    matrix(c(rep(x4, 2), rep(y4, 2), z4), ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
  }

  ncone <- attr(coldistres, "ncone")

  if (as.numeric(ncone) < 2 || as.numeric(ncone) > 4) {
    stop("only methods for di-, tri- and tetrachromatic models are implemented so far", call. = FALSE)
  }

  references <- attr(coldistres, "resref")
  references <- references[intersect(
    grep("jnd2xyzrrf", references$patch1, invert = TRUE, fixed = TRUE),
    grep("jnd2xyzrrf", references$patch2, fixed = TRUE)
  ), ]

  # Strip 'lum' column if it's all NA
  if (all(is.na(coldistres$dL))) {
    coldistres <- coldistres[, names(coldistres) != "dL"]
  }

  combined <- rbind(coldistres, references)

  colmat <- coldist2mat(combined)

  cdmat <- colmat[["dS"]]

  coords <- matrix(NA,
    nrow = nrow(cdmat), ncol = as.numeric(ncone) - 1,
    dimnames = list(row.names(cdmat), c("x", "y", "z")[seq(as.numeric(ncone) - 1)])
  )

  ptnames <- rownames(coords)

  if (ncone == "2") {
    # 2 cones, only 1 dimension

    # first point
    coords[ptnames[1], ] <- 0

    # second point
    coords[ptnames[2], ] <- cdmat[ptnames[1], ptnames[2]]

    # subsequent points
    coords[c(ptnames[-(1:2)]), ] <- do.call(rbind, lapply(ptnames[-(1:2)], function(x) {
      pos2(
        cdmat[ptnames[1], ptnames[2]],
        cdmat[ptnames[1], x],
        cdmat[ptnames[2], x]
      )
    }))
  }

  if (ncone == "3") {
    # first point
    coords[ptnames[1], ] <- c(0, 0)

    # second point
    coords[ptnames[2], ] <- c(cdmat[ptnames[1], ptnames[2]], 0)

    # third point
    coords[ptnames[3], ] <- pos3(
      cdmat[ptnames[1], ptnames[2]],
      cdmat[ptnames[1], ptnames[3]],
      cdmat[ptnames[2], ptnames[3]]
    )[1, ]


    # subsequent points
    positions <- lapply(ptnames[-(1:3)], function(x) {
      pos3(
        cdmat[ptnames[1], ptnames[2]],
        cdmat[ptnames[1], x],
        cdmat[ptnames[2], x]
      )
    })
    names(positions) <- ptnames[-(1:3)]


    eucdis <- lapply(positions, function(x) dist(rbind(x, coords[ptnames[3], ]))[c(2, 3)])

    whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - cdmat[ptnames[3], x])))
    names(whichdist) <- names(eucdis)

    coords[names(eucdis), ] <- do.call(
      rbind,
      lapply(names(eucdis), function(x) positions[[x]][whichdist[[x]], ])
    )
  }

  if (ncone == "4") {
    # first point
    coords[ptnames[1], ] <- c(0, 0, 0)

    # second point
    coords[ptnames[2], ] <- c(cdmat[ptnames[1], ptnames[2]], 0, 0)

    # third point
    thirdpointxy <- pos3(
      cdmat[ptnames[1], ptnames[2]],
      cdmat[ptnames[1], ptnames[3]],
      cdmat[ptnames[2], ptnames[3]]
    )[1, ]

    coords[ptnames[3], ] <- c(thirdpointxy, 0)

    # fourth point
    fourthpointxyz <- pos4(
      cdmat[ptnames[1], ptnames[2]],
      cdmat[ptnames[1], ptnames[4]],
      cdmat[ptnames[2], ptnames[4]],
      cdmat[ptnames[3], ptnames[4]]
    )[1, ]

    coords[ptnames[4], ] <- fourthpointxyz

    # subsequent points
    positions <- lapply(ptnames[-(1:4)], function(x) {
      pos4(
        cdmat[ptnames[1], ptnames[2]],
        cdmat[ptnames[1], x],
        cdmat[ptnames[2], x],
        cdmat[ptnames[3], x]
      )
    })
    names(positions) <- ptnames[-(1:4)]


    eucdis <- lapply(positions, function(x) dist(rbind(x, coords[4, ]))[c(2, 3)])

    whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - cdmat[ptnames[4], x])))
    names(whichdist) <- names(eucdis)

    coords[names(eucdis), ] <- do.call(
      rbind,
      lapply(names(eucdis), function(x) positions[[x]][whichdist[[x]], ])
    )
  }

  if ("dL" %in% names(colmat)) {
    ldmat <- colmat[["dL"]]
    coords <- cbind(coords, lum = 0)

    # first point
    coords[ptnames[1], "lum"] <- 0

    # second point
    coords[ptnames[2], "lum"] <- ldmat[ptnames[1], ptnames[2]]

    # subsequent points
    coords[c(ptnames[-(1:2)]), "lum"] <- do.call(rbind, lapply(ptnames[-(1:2)], function(x) {
      pos2(
        ldmat[ptnames[1], ptnames[2]],
        ldmat[ptnames[1], x],
        ldmat[ptnames[2], x]
      )
    }))
    # invert if darkest point is positive


    # center on achromatic point
    coords[, "lum"] <- coords[, "lum"] - coords["jnd2xyzrrf.achro", "lum"]
    # coords[,'lum'] <- coords[,'lum'] - coords[darker,'lum']
  }

  # Center variables
  centroids <- colMeans(coords[grep("jnd2xyzrrf", rownames(coords), invert = TRUE, fixed = TRUE), , drop = FALSE])
  if (center) {
    coords <- sweep(coords, 2, centroids, "-")
  }

  jnd2xyzrrf.ctrd <- colMeans(coords[grep("jnd2xyzrrf", rownames(coords), invert = TRUE, fixed = TRUE), , drop = FALSE])

  chromcoords <- as.data.frame(coords[grep("jnd2xyzrrf", rownames(coords), invert = TRUE, fixed = TRUE), , drop = FALSE])

  refstosave <- as.data.frame(rbind(
    coords[grep("jnd2xyzrrf", rownames(coords), fixed = TRUE), , drop = FALSE], jnd2xyzrrf.ctrd
  ))

  attr(chromcoords, "class") <- c("colspace", "jnd2xyz", "data.frame")
  attr(chromcoords, "resref") <- refstosave

  if (rotate) {
    rotcenter <- match.arg(rotcenter)
    rotarg <- list(
      jnd2xyzres = chromcoords, center = rotcenter,
      ref1 = ref1, ref2 = ref2, axis1 = axis1, axis2 = axis2
    )

    chromcoords <- do.call(jndrot, rotarg)
  }

  chromcoords
}
