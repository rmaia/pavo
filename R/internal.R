# #' @export loadrgl

# loadrgl <- function(){
# # load RGL, and attempt install if not found
# if (!require('rgl',character.only = TRUE))
# {
# message('package ', dQuote('rgl'), ' not found; attempting install...')
# install.packages('rgl',dep=TRUE)

# if(!require('rgl',character.only = TRUE))
# stop(dQuote('rgl'), " package is required and could not be installed; please install and try again")
# }
# }

##################################
# START RECEPTOR NOISE FUNCTIONS #
##################################

newreceptornoise.neural <- function(dat, n, weber, weber.ref, res) {
  reln <- n / sum(n)
  v <- weber * sqrt(reln[weber.ref])
  e <- setNames(v / sqrt(reln), colnames(dat))

  #############
  # NUMERATOR #
  #############

  # all n-2 combinations (first part numerator)
  n1combs <- combn(colnames(dat), dim(dat)[2] - 2)

  # get those combinations of ei and prod(ei)^2

  num1 <- setNames(
    apply(n1combs, 2, function(x) prod(e[x])^2),
    apply(n1combs, 2, paste, collapse = "")
  )

  # remaining 2 combinations (second part numerator)
  n2combs <- apply(n1combs, 2, function(x) colnames(dat)[ !colnames(dat) %in% x ])

  # f_d and f_e

  deltaqiqj <- lapply(1:length(num1), function(y)
    t(apply(res, 1, function(x)
      dat[x[1], n2combs[, y]] - dat[x[2], n2combs[, y]])))

  names(deltaqiqj) <- apply(n2combs, 2, paste, collapse = "")

  # (f_d-f_e)^2

  num2 <- do.call("cbind", lapply(deltaqiqj, function(x) apply(x, 1, function(z) diff(z)^2)))

  # (e_abc)^2*(f_d-f_e)^2

  etimesq <- num2 %*% diag(num1)

  # sum numerator

  numerator <- rowSums(etimesq)

  ###############
  # DENOMINATOR #
  ###############

  # all n-1 combinations
  dcombs <- combn(colnames(dat), dim(dat)[2] - 1)

  den <- setNames(
    apply(dcombs, 2, function(x) prod(e[x])^2),
    apply(dcombs, 2, paste, collapse = "")
  )

  denominator <- sum(den)

  ###########
  # DELTA S #
  ###########

  sqrt(numerator / denominator)
}



newreceptornoise.quantum <- function(dat, n, weber, weber.ref, res, qndat) {
  reln <- n / sum(n)
  v <- weber * sqrt(reln[weber.ref])

  ept1 <- setNames(v^2 / reln, colnames(dat))
  ept2 <- 2 / t(apply(res, 1, function(x) qndat[x[1], ] + qndat[x[2], ]))
  e <- sqrt(sweep(ept2, 2, ept1, "+"))


  #############
  # NUMERATOR #
  #############

  # all n-2 combinations (first part numerator)
  n1combs <- combn(colnames(dat), dim(dat)[2] - 2)

  # get those combinations of ei and prod(ei)^2

  num1 <- do.call("rbind", lapply(1:dim(res)[1], function(z)
    apply(n1combs, 2, function(x) prod(e[z, x])^2)))
  colnames(num1) <- apply(n1combs, 2, paste, collapse = "")

  # remaining 2 combinations (second part numerator)
  n2combs <- apply(n1combs, 2, function(x) colnames(dat)[ !colnames(dat) %in% x ])

  # f_d and f_e

  deltaqiqj <- lapply(1:dim(n1combs)[2], function(y)
    t(apply(res, 1, function(x)
      dat[x[1], n2combs[, y]] - dat[x[2], n2combs[, y]])))

  names(deltaqiqj) <- apply(n2combs, 2, paste, collapse = "")

  # (f_d-f_e)^2

  num2 <- do.call("cbind", lapply(deltaqiqj, function(x)
    apply(x, 1, function(z) diff(z)^2)))

  # (e_abc)^2*(f_d-f_e)^2

  etimesq <- num2 * num1

  # sum numerator

  numerator <- rowSums(etimesq)

  ###############
  # DENOMINATOR #
  ###############

  # all n-1 combinations
  dcombs <- combn(colnames(dat), dim(dat)[2] - 1)

  den <- do.call("rbind", lapply(1:dim(res)[1], function(z)
    apply(dcombs, 2, function(x) prod(e[z, x])^2)))
  colnames(den) <- apply(dcombs, 2, paste, collapse = "")

  denominator <- rowSums(den)

  ###########
  # DELTA S #
  ###########

  sqrt(numerator / denominator)
}

# achromatic functions

ttdistcalcachro <- function(f1, f2, weber.achro) {
  dq1 <- f1[length(f1)] - f2[length(f1)]
  dq1 <- as.numeric(dq1)
  w <- weber.achro
  round(abs(dq1 / w), 7)
}

qn.ttdistcalcachro <- function(f1, f2, qn1, qn2, weber.achro) {
  dq1 <- f1[length(f1)] - f2[length(f1)]
  dq1 <- as.numeric(dq1)
  w <- sqrt((weber.achro)^2 + (2 / (qn1[length(qn1)] + qn2[length(qn1)])))
  round(abs(dq1 / w), 7)
}


################################
# END RECEPTOR NOISE FUNCTIONS #
################################

# 2d Euclidean distance
euc2d <- function(coord1, coord2) {
  as.numeric(round(sqrt(abs(coord1["x"] - coord2["x"])^2 + abs(coord1["y"] - coord2["y"])^2), 7))
}

# 2d Euclidean distance in segment space
seg2d <- function(coord1, coord2) {
  as.numeric(round(sqrt(abs(coord1["MS"] - coord2["MS"])^2 + abs(coord1["LM"] - coord2["LM"])^2), 7))
}

# Achromatic contrast in segment space
achroseg <- function(coord1, coord2) {
  as.numeric(abs(coord1["B"] - coord2["B"]))
}

# Achromatic 'green' receptor contrast in the hexagon
achrohex <- function(coord1, coord2) {
  as.numeric(round(coord1["l"] / coord2["l"], 7))
}

# Achromatic contrast in cielab
achrolab <- function(coord1, coord2) {
  as.numeric(abs(coord1["L"] - coord2["L"]))
}

# 2d Euclidean distances in CIELAB
lab2d <- function(coord1, coord2) {
  as.numeric(round(sqrt(abs(coord1["L"] - coord2["L"])^2 + abs(coord1["a"] - coord2["a"])^2 +
    abs(coord1["b"] - coord2["b"])^2), 7))
}

# CIE2000 colour distance for CIELCh (LOLWAT)
cie2000 <- function(coord1, coord2) {

  # Lightness difference
  dL <- coord2["L"] - coord1["L"]

  # Mean lightness
  mL <- (coord2["L"] + coord1["L"]) / 2

  # Chroma difference
  dC <- coord2["C"] - coord1["C"]

  # Mean chroma
  mC <- (coord2["C"] + coord1["C"]) / 2

  # Hue difference
  if (coord1["h"] - coord2["h"] <= 180) {
    dh <- coord2["h"] - coord1["h"]
  } else if (coord1["h"] - coord2["h"] > 180 & coord2["h"] <= coord1["h"]) {
    dh <- coord2["h"] + coord1["h"] + 360
  } else if (coord1["h"] - coord2["h"] > 180 & coord2["h"] > coord1["h"]) {
    dh <- coord2["h"] + coord1["h"] - 360
  }

  # Mean hue
  if (abs(coord2["h"] - coord1["h"]) <= 180) {
    mh <- (coord2["h"] + coord1["h"]) / 2
  } else if (abs(coord2["h"] - coord1["h"]) > 180 & coord2["h"] + coord1["h"] < 360) {
    mh <- (coord2["h"] + coord1["h"] + 360) / 2
  } else if (abs(coord2["h"] - coord1["h"]) > 180 & coord2["h"] + coord1["h"] >= 360) {
    mh <- (coord2["h"] + coord1["h"] - 360) / 2
  }

  t <- 1 - (0.17 * cos(mh - 30)) + (0.24 * cos(2 * mh)) + (0.32 * cos(3 * mh + 6)) - (0.2 * cos(4 * mh - 63))

  sL <- 1 + ((0.17 * (mL - 50)^2) / sqrt(20 + (mL - 50)^2))
  sC <- 1 + 0.045 * mC
  sH <- 1 + 0.015 * mC * t

  Rt <- -2 * sqrt(mC^7 / (mC^7 + 25^7)) * sin(60 * exp(-1 * (((mh - 275) / 25)^2)))

  as.numeric(round(sqrt((dL / sL)^2 + (dC / sC)^2 + (dh / sH)^2 + (Rt * (dC / sC) * (dh / sH)))), 7)
}

# Manhattan distance
bloc2d <- function(coord1, coord2) {
  as.numeric(round(abs(coord1["x"] - coord2["x"]) + abs(coord1["y"] - coord2["y"])), 7)
}

#######################
# END OTHER DISTANCES #
#######################

#####################
# SUMMARY VARIABLES #
#####################

huedisp <- function(tcsres) {
  ind <- t(combn(nrow(tcsres), 2))
  apply(ind, 1, function(x)
    acos((cos(tcsres[x[1], "h.phi"]) * cos(tcsres[x[2], "h.phi"]) * cos(tcsres[x[1], "h.theta"] -
      tcsres[x[2], "h.theta"])) + (sin(tcsres[x[1], "h.phi"]) * sin(tcsres[x[2], "h.phi"]))))
}


tcssum <- function(tcsres) {
  # centroid
  centroid <- colMeans(tcsres[c("u", "s", "m", "l")])

  # color span
  colspan.m <- mean(dist(tcsres[, c("x", "y", "z")]))
  colspan.v <- var(dist(tcsres[, c("x", "y", "z")]))

  # color volume
  if (nrow(tcsres) > 3) {
    c.vol <- convhulln(tcsres[, c("x", "y", "z")], "FA")$vol
  } else {
    c.vol <- NA
  }

  # relative color volume
  if (nrow(tcsres) > 3) {
    # Exact formula for the volume of a regular tetrahedron inscribed in a
    # circle of radius (3/4)
    tot.c.vol <- (3 / sqrt(6))^3 / (6 * sqrt(2))
    rel.c.vol <- c.vol / tot.c.vol
  } else {
    rel.c.vol <- NA
  }

  # hue disparity
  hdisp.m <- mean(huedisp(tcsres))
  hdisp.v <- var(huedisp(tcsres))

  # summary of achieved chroma
  mean.ra <- mean(tcsres$r.achieved)
  max.ra <- max(tcsres$r.achieved)

  res.c <- c(centroid, c.vol, rel.c.vol, colspan.m, colspan.v, hdisp.m, hdisp.v, mean.ra, max.ra)
  names(res.c) <- c(
    "centroid.u", "centroid.s", "centroid.m", "centroid.l",
    "c.vol", "rel.c.vol", "colspan.m", "colspan.v", "huedisp.m", "huedisp.v",
    "mean.ra", "max.ra"
  )

  res.c
}

# TODO (Tom): These are a couple of functions that do what should be simple things in an
# ugly way because my maths/programming is bad. Needs to be fixed.

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