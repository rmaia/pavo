#' Colour volume overlap
#'
#' Compute colour volume overlap in 3D with alphashapes
#'
#' @inheritParams voloverlap
#'
#' @return a data.frame with the following columns:
#' - `s_in1, s_in2` the number of sampled points that fall within each of the
#' volumes individually.
#' - `s_inboth` the number of sampled points that fall within both volumes.
#' - `s_ineither` the number of points that fall within either of the volumes.
#' - `psmallest` the proportion of points that fall within both volumes divided
#' by the number of points that fall within the smallest volume.
#' - `pboth` the proportion of points that fall within both volumes divided by
#' the total number of points that fall within both volumes.
#'
#' @examples
#' data(sicalis)
#' tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
#' tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
#' tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")
#' \donttest{
#' pavo:::overlap3d(tcs.sicalis.T, tcs.sicalis.B, avalue = 1)
#' pavo:::overlap3d(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE, avalue = 0.1)
#' pavo:::overlap3d(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE, col = seq_len(3), avalue = 0.1)
#' }
#'
#' @importFrom stats runif
#'
#' @references 
#' Gruson H. 2020. Estimation of colour volumes as concave hypervolumes using 
#'  \ifelse{html}{\out{&alpha;}}{\eqn{$\alpha$}{alpha}}‐shapes. Methods in 
#'  Ecology and Evolution, early view \doi{10.1111/2041-210X.13398}

overlap3d <- function(colsp1, colsp2, avalue1, avalue2 , plot = FALSE, 
                      interactive = TRUE, col = c("blue", "red", "darkgrey"),
                      fill = FALSE, new = TRUE, nsamp = 1000, psize = 0.001,
                      lwd = 1, ...) {

  if (!interactive) {
    warning("interactive = FALSE has not been implemented yet, falling back to",
            " interactive plot.")
  }

  dat1 <- colsp1[, c("x", "y", "z")]

  dat2 <- colsp2[, c("x", "y", "z")]

  shape1 <- alphashape3d::ashape3d(as.matrix(dat1), avalue1)
  shape2 <- alphashape3d::ashape3d(as.matrix(dat2), avalue2)

  vol1 <- alphashape3d::volume_ashape3d(shape1)
  vol2 <- alphashape3d::volume_ashape3d(shape2)

  # sample random points
  pmin <- apply(rbind(dat1, dat2), 2, min)
  pmax <- apply(rbind(dat1, dat2), 2, max)

  samples <- apply(rbind(pmin, pmax), 2, function(x) runif(nsamp, x[1], x[2]))

  invol1 <- alphashape3d::inashape3d(shape1, points = samples)
  invol2 <- alphashape3d::inashape3d(shape2, points = samples)

  # how many points are in each category

  s_in1 <- sum(invol1)
  s_in2 <- sum(invol2)

  s_inboth <- sum(invol1 & invol2)

  s_ineither <- sum(invol1 | invol2)

  # points in both relative points in smallest

  psmallest <- s_inboth / c(s_in1, s_in2)[which.min(c(vol1, vol2))]

  # points in both relative to total points in both

  pboth <- s_inboth / s_ineither

  res <- data.frame(vol1, vol2, s_in1, s_in2, s_inboth, s_ineither, psmallest, pboth)

  ############
  # PLOT BEGIN#
  ############
  if (plot) {
    if (length(col) < 3) {
      col <- c(rep(col, 2)[seq_len(2)], "darkgrey")
    }
    # check if rgl is installed and loaded
    if (!requireNamespace("rgl", quietly = TRUE)) {
      stop(dQuote("rgl"), " package needed for interactive plots. Please install it, or use interactive=FALSE.",
           call. = FALSE
      )
    }

    if (!isNamespaceLoaded("rgl")) {
      requireNamespace("rgl")
    }

    if (new) {
      rgl::open3d(FOV = 1, mouseMode = c("zAxis", "xAxis", "zoom"))
    }

    tcsvol(colsp1, type = "alpha", avalue = avalue1 ,col = col[1], fill = FALSE)
    tcsvol(colsp2, type = "alpha", avalue = avalue2, col = col[2], fill = FALSE)

    rgl::spheres3d(samples[which(invol1 & !invol2), ],
                   type = "s",
                   lit = FALSE, radius = psize, col = col[1]
    )
    rgl::spheres3d(samples[which(invol2 & !invol1), ],
                   type = "s",
                   lit = FALSE, radius = psize, col = col[2]
    )

    if (s_inboth > 0) {
      rgl::spheres3d(samples[which(invol1 & invol2), ],
                     type = "s",
                     lit = FALSE, radius = psize, col = col[3]
      )
    }
  }
  ##########
  # PLOT END#
  ##########

  res
}
