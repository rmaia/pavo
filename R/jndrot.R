#' Rotate Cartesian coordinates obtained from \code{jnd2xyz}
#'
#' Rotates the Cartesian coordinates obtained from \code{jnd2xyz}
#'
#' @param jnd2xyzres (required) the output from a \code{jnd2xyz} call.
#' @param center should the vectors for rotation be centered in the achromatic
#' center ("achro") or the data centroid ("mean", the default)?
#' @param ref1 the cone to be used as a the first reference. May be NULL
#' (for no first rotation in the 3-dimensional case) or must match name
#' in the original data that was used for \code{coldist}. Defaults to 'l'.
# " (only used if data has 2 or 3 dimensions)
#' @param ref2 the cone to be used as a the second reference.May be NULL
#' (for no first rotation in the 3-dimensional case) or must match name
#' in the original data that was used for \code{coldist}. Defaults to 'u'.
#' (only used if data has 3 dimensions).
#' @param axis1 A vector of length 3 composed of 0's and 1's, with
#' 1's representing the axes (x,y,z) to rotate around. Defaults to c(1,1,0), such
#' that the rotation aligns with the xy plane (only used if data has 2 or 3 dimensions).
#' Ignored if \code{ref1} is NULL (in 3-dimensional case only)
#' @param axis2 A vector of length 3 composed of 0's and 1's, with
#' 1's representing the axes (x,y,z) to rotate around. Defaults to c(0,0,1), such
#' that the rotation aligns with the z axis (only used if data has 3 dimensions).
#' Ignored if \code{ref2} is NULL (in 3-dimensional case only)
#'
#' @examples \dontrun{
#' data(flowers)
#' vis.flowers <- vismodel(flowers)
#' cd.flowers <- coldist(vis.flowers)
#' jndrot(jnd2xyz(cd.flowers))
#' }
#'
#' @author Rafael Maia \email{rm72@zips.uakron.edu}
#'
#' @export
#'
#' @keywords internal

jndrot <- function(jnd2xyzres, center = c("mean", "achro"), ref1 = "l", ref2 = "u", axis1 = c(1, 1, 0), axis2 = c(0, 0, 1)) {
  if (!"jnd2xyz" %in% class(jnd2xyzres)) {
    stop('object must be a result from the "jnd2xyz" function')
  }

  if (!is.null(ref1) && !paste0("jnd2xyzrrf.", ref1) %in% rownames(attr(jnd2xyzres, "resref"))) {
    stop('"ref1" does not match the name of a photoreceptor; must be one of: ',
      paste0(gsub("jnd2xyzrrf.", "", rownames(attr(jnd2xyzres, "resref")))
      [-c(1, length(rownames(attr(jnd2xyzres, "resref"))))], collapse = ", ")
      ,
      call. = FALSE
    )
  }

  if (!is.null(ref2) && all(c("x", "y", "z") %in% colnames(jnd2xyzres)) && !paste0("jnd2xyzrrf.", ref2) %in% rownames(attr(jnd2xyzres, "resref"))) {
    stop('"ref2" does not match the name of a photoreceptor; must be one of: ',
      paste0(gsub("jnd2xyzrrf.", "", rownames(attr(jnd2xyzres, "resref")))
      [-c(1, length(rownames(attr(jnd2xyzres, "resref"))))], collapse = ", ")
      ,
      call. = FALSE
    )
  }

  center <- match.arg(center)

  # helper functions
  vectormag <- function(x) sqrt(sum(x^2))
  vectornorm <- function(x) matrix(as.numeric(x / vectormag(x)), ncol = 1)
  vectorcross <- function(a, b) {
    i <- c(2, 3, 1)
    j <- c(3, 1, 2)
    a[i] * b[j] - a[j] * b[i]
  }

  coordsall <- as.matrix(rbind(jnd2xyzres, attr(jnd2xyzres, "resref")))

  # remove lum column if there is one
  coords <- coordsall[, colnames(coordsall) %in% c("x", "y", "z"), drop = FALSE]

  # one dimension
  if (round(sum(c("x", "y", "z") %in% colnames(coords))) == 1) {
    if (coords[dim(coords)[1], "x"] < coords[dim(coords)[1] - 1, "x"]) {
      coords[, "x"] <- coords[, "x"] * -1
    }

    res <- coords
    colnames(res) <- colnames(coords)
  }

  # two dimensions
  if (round(sum(c("x", "y", "z") %in% colnames(coords))) == 2) {
    coords <- cbind(coords, tempcol = 0)

    if (length(axis1) != 3) {
      stop('"axis1" must be a vector of length 3')
    }

    cent <- switch(center,
      achro = coords["jnd2xyzrrf.achro", ],
      mean = coords["jnd2xyzrrf.ctrd", ]
    )

    aa <- vectornorm(coords[grep(paste0("jnd2xyzrrf.", ref1), rownames(coords)), ] -
      cent)
    bb <- vectornorm(axis1)
    daabb <- sum(aa * bb)
    ncaabb <- vectormag(vectorcross(aa, bb))
    GG <- rbind(
      c(daabb, -ncaabb, 0),
      c(ncaabb, daabb, 0),
      c(0, 0, 1)
    )
    FF <- cbind(
      aa,
      vectornorm(bb - daabb * aa),
      vectorcross(bb, aa)
    )

    RR <- FF %*% GG %*% solve(FF)

    res <- sweep(coords, 2, cent, "-")
    res <- t(apply(res, 1, function(x) RR %*% x))
    # res <- sweep(res, 2, coords['jnd2xyzrrf.achro',], '+')

    res <- res[, -dim(res)[2]]
    coords <- coords[, -dim(coords)[2]]
    colnames(res) <- colnames(coords)
  }

  # three dimensions
  if (round(sum(c("x", "y", "z") %in% colnames(coords))) == 3) {
    if (length(axis1) != 3) {
      stop('"axis1" must be a vector of length 3')
    }

    cent <- switch(center,
      achro = coords["jnd2xyzrrf.achro", ],
      mean = coords["jnd2xyzrrf.ctrd", ]
    )

    # first rotation
    if (!is.null(ref1)) {
      aa <- vectornorm(coords[grep(paste0("jnd2xyzrrf.", ref1), rownames(coords)), ] -
        cent)
      bb <- vectornorm(axis1)
      daabb <- sum(aa * bb)
      ncaabb <- vectormag(vectorcross(aa, bb))
      GG <- rbind(
        c(daabb, -ncaabb, 0),
        c(ncaabb, daabb, 0),
        c(0, 0, 1)
      )
      FF <- cbind(
        aa,
        vectornorm(bb - daabb * aa),
        vectorcross(bb, aa)
      )

      RR <- FF %*% GG %*% solve(FF)

      res <- sweep(coords, 2, cent, "-")
      res <- t(apply(res, 1, function(x) RR %*% x))
      # res <- sweep(res, 2, coords['jnd2xyzrrf.achro',], '+')
    } else {
      res <- coords
    }



    # second rotation

    if (!is.null(ref2)) {
      if (length(axis2) != 3) {
        stop('"axis2" must be a vector of length 3')
      }

      aa <- vectornorm(res[grep(paste0("jnd2xyzrrf.", ref2), rownames(res)), ] -
        cent)
      bb <- vectornorm(axis2)
      daabb <- sum(aa * bb)
      ncaabb <- vectormag(vectorcross(aa, bb))
      GG <- rbind(
        c(daabb, -ncaabb, 0),
        c(ncaabb, daabb, 0),
        c(0, 0, 1)
      )
      FF <- cbind(
        aa,
        vectornorm(bb - daabb * aa),
        vectorcross(bb, aa)
      )

      RR <- FF %*% GG %*% solve(FF)

      res <- sweep(res, 2, cent, "-")
      res <- t(apply(res, 1, function(x) RR %*% x))
      # res <- sweep(res, 2, coords['jnd2xyzrrf.achro',], '+')
    }

    colnames(res) <- colnames(coords)
  }

  if ("lum" %in% colnames(jnd2xyzres)) {
    res <- cbind(res, lum = coordsall[, "lum"])
  }

  chromcoords <- res[grep("jnd2xyzrrf", rownames(res), invert = TRUE), , drop = FALSE]

  chromcoords <- as.data.frame(chromcoords)

  refstosave <- as.data.frame(res[grep("jnd2xyzrrf", rownames(res)), , drop = FALSE])

  attr(chromcoords, "class") <- c("colspace", "jnd2xyz", "data.frame")
  attr(chromcoords, "resref") <- refstosave

  chromcoords
}
