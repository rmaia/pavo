.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to pavo 2! Take a look at the latest features (and update your bibliography) in our recent publication: Maia R., Gruson H., Endler J. A., White T. E. (2019) pavo 2: new tools for the spectral and spatial analysis of colour in R. Methods in Ecology and Evolution, 10, 1097-1107.")
}

#####################
# SUMMARY VARIABLES #
#####################

#' @importFrom utils combn
huedisp <- function(tcsres) {
  if (nrow(tcsres) == 1) {
    return(NA)
  }
  # This function can probably also be expressed with x,y,z or u,s,m,l, which
  # might help write a more efficient code using linear algebra libs.
  alphas <- combn(nrow(tcsres), 2, function(x) {
    phi <- tcsres[x, "h.phi"]
    theta <- tcsres[x, "h.theta"]

    prod(cos(phi), cos(diff(theta))) + prod(sin(phi))
  })
  acos(alphas)
}


#' @importFrom geometry convhulln
#' @importFrom stats dist var
tcssum <- function(tcsres) {
  # centroid
  centroid <- colMeans(tcsres[c("u", "s", "m", "l")])

  # color span
  colspan <- dist(tcsres[, c("x", "y", "z")])
  colspan.m <- mean(colspan)
  colspan.v <- var(colspan)

  if (nrow(tcsres) > 3) {
    # color volume
    c.vol <- convhulln(tcsres[, c("x", "y", "z")], "FA")$vol

    # Exact formula for the volume of a regular tetrahedron inscribed in a
    # circle of radius (3/4)
    tot.c.vol <- sqrt(3) / 8

    # FIXME: there is a bug in alphashape3d which will sometimes fail on legit
    # calls, such as
    # summary(colspace(vismodel(flowers)), by = 4)
    # so we wrap it in tryCatch() to prevent the error from trickling down in
    # summary.colspace()
    if (requireNamespace("alphashape3d", quietly = TRUE)) {
      a.vol <- tryCatch(
        {
          astar <- find_astar(as.matrix(tcsres[, c("x", "y", "z")]))
          ashape <- alphashape3d::ashape3d(as.matrix(tcsres[, c("x", "y", "z")]), astar)
          alphashape3d::volume_ashape3d(ashape)
        },
        error = function(e) {
          warning("There was an error in the computation of the alpha-shape volume", call. = FALSE)
          return(NA_real_)
        }
      )
    } else {
      message(
        "Please install the 'alphashape3d' package to get the value of a.vol"
      )
      a.vol <- NA_real_
    }

    # relative color volume
    rel.c.vol <- c.vol / tot.c.vol
  } else {
    c.vol <- NA_real_
    rel.c.vol <- NA_real_
    a.vol <- NA_real_
  }

  # hue disparity
  hdisp <- huedisp(tcsres)
  hdisp.m <- mean(hdisp)
  hdisp.v <- var(hdisp)

  # summary of achieved chroma
  mean.ra <- mean(tcsres$r.achieved)
  max.ra <- max(tcsres$r.achieved)

  res.c <- c(
    centroid,
    c.vol, rel.c.vol,
    colspan.m, colspan.v,
    hdisp.m, hdisp.v,
    mean.ra, max.ra,
    a.vol
  )

  names(res.c) <- c(
    "centroid.u", "centroid.s", "centroid.m", "centroid.l",
    "c.vol", "rel.c.vol", "colspan.m", "colspan.v", "huedisp.m", "huedisp.v",
    "mean.ra", "max.ra",
    "a.vol"
  )

  res.c
}

# TODO (Tom): These are a couple of functions that do what should be simple things in an
# ugly way because my maths/programming is bad.

# Calculate hexagon hue angle (in degrees, moving clockwise, with 1200 as 0)
# in the colour hexagon
angle360 <- function(x, y) {
  theta <- 90 - (atan2(y, x) * (180 / pi))
  if (theta < 0) {
    theta <- theta + 360
  }

  theta
}

# Calculate the coarse hexagon sector
coarse_sec <- function(x) {
  if (isTRUE(x == 0)) {
    return("achro")
  }
  if (isTRUE(x >= 30 && x < 90)) {
    return("bluegreen")
  }
  if (isTRUE(x >= 90 && x < 150)) {
    return("green")
  }
  if (isTRUE(x >= 150 && x < 210)) {
    return("uvgreen")
  }
  if (isTRUE(x >= 210 && x < 270)) {
    return("uv")
  }
  if (isTRUE(x >= 270 && x < 330)) {
    return("uvblue")
  }
  if (isTRUE(x >= 330 || x < 30)) {
    return("blue")
  }
}

## Weighted stats
weightmean <- function(x, wt) {
  s <- which(is.finite(x * wt))
  wt <- wt[s]
  x <- x[s]
  sum(wt * x) / sum(wt)
}

weightsd <- function(x, wt) {
  s <- which(is.finite(x + wt))
  wt <- wt[s]
  x <- x[s]
  xbar <- weightmean(x, wt)
  sqrt(sum(wt * (x - xbar)^2) * (sum(wt) / (sum(wt)^2 - sum(wt^2))))
}

## Circular statistics
circmean <- function(x) {
  sinr <- sum(sin(x))
  cosr <- sum(cos(x))
  circmean <- atan2(sinr, cosr)
  circmean
}

circsd <- function(x) {
  n <- length(x)
  sinr <- sum(sin(x))
  cosr <- sum(cos(x))
  result <- sqrt(sinr^2 + cosr^2) / n
  circsd <- sqrt(-2 * log(result))
  circsd
}

circvar <- function(x) {
  n <- length(x)
  sinr <- sum(sin(x))
  cosr <- sum(cos(x))
  circvar <- 1 - (sqrt(sinr^2 + cosr^2) / n)
  circvar
}
