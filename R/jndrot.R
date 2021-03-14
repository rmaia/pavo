#' Rotate Cartesian coordinates obtained from [jnd2xyz()]
#'
#' @param jnd2xyzres (required) the output from a [jnd2xyz()] call.
#' @param center should the vectors for rotation be centered in the achromatic
#' center ("achro") or the data centroid ("mean", the default)?
#' @inheritParams jnd2xyz
#' @examples
#' # Load floral reflectance spectra
#' data(flowers)
#' 
#' # Estimate quantum catches visual phenotype of a Blue Tit
#' vis.flowers <- vismodel(flowers, visual = 'bluetit')
#' 
#' # Estimate noise-weighted colour distances between all flowers 
#' cd.flowers <- coldist(vis.flowers)
#' 
#' # Convert points to Cartesian coordinates in which Euclidean distances are 
#' # noise-weighted, before rotating them about the data centroid
#' jndrot(jnd2xyz(cd.flowers))
#' 
#' @author Rafael Maia \email{rm72@zips.uakron.edu}
#'
#' @export
#'
#' @keywords internal

jndrot <- function(jnd2xyzres, center = c("mean", "achro"), ref1 = "l", ref2 = "u", axis1 = c(1, 1, 0), axis2 = c(0, 0, 1)) {
  if (!inherits(jnd2xyzres, "jnd2xyz")) {
    stop('object must be a result from the "jnd2xyz" function')
  }

  if (!is.null(ref1) && !paste0("jnd2xyzrrf.", ref1) %in% rownames(attr(jnd2xyzres, "resref"))) {
    stop('"ref1" does not match the name of a photoreceptor; must be one of: ',
      paste0(gsub("jnd2xyzrrf.", "", rownames(attr(jnd2xyzres, "resref")))
      [-c(1, length(rownames(attr(jnd2xyzres, "resref"))))], collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(ref2) && all(c("x", "y", "z") %in% colnames(jnd2xyzres)) &&
    !paste0("jnd2xyzrrf.", ref2) %in% rownames(attr(jnd2xyzres, "resref"))) {
    stop('"ref2" does not match the name of a photoreceptor; must be one of: ',
      paste0(gsub("jnd2xyzrrf.", "", rownames(attr(jnd2xyzres, "resref")))
      [-c(1, length(rownames(attr(jnd2xyzres, "resref"))))], collapse = ", "),
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
    daabb <- drop(crossprod(aa, bb))
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
    res <- tcrossprod(res, RR)
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
      daabb <- drop(crossprod(aa, bb))
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
      res <- tcrossprod(res, RR)
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
      daabb <- drop(crossprod(aa, bb))
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
      res <- tcrossprod(res, RR)
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
