#' Colour volume overlap
#'
#' Calculates the overlap between the volumes defined by two sets of points in cartesian
#' space.
#'
#' @importFrom geometry convhulln intersectn
#'
#' @export
#'
#' @param colsp1,colsp2 (required) data frame, possibly a result from the [colspace()]
#' function, containing
#' values for the 'x', 'y' (and possibly 'z') coordinates as columns (labeled as such)
#' @param type if "convex", the colour volume is plotted using a convex hull
#'   and if "alpha", it is plotted using alphashapes.
#' @param avalue if `type = alpha`, the alpha parameter values for `colsp1` and
#' `colsp2` respectively to compute the alphashapes. Can be a numeric of length
#' one if the same value is used in both cases. `avalue = "auto"` (default)
#' finds and use the \eqn{\alpha^*}{alpha*} value as defined in Gruson (2020).
#' @param nsamp if `type = "alpha"`, the number of points to be sampled for the
#' Monte Carlo computation. Stoddard & Stevens(2011) use around 750,000 points,
#'  but more or fewer might be required depending on the
#' degree of overlap.
#' @param psize if `type = "alpha"` and `plot = TRUE`, sets the size to plot the points
#' used in the Monte Carlo computation.
#' @param plot logical. Should the volumes and points be plotted? (defaults to `FALSE`).
#' This only works for tetrahedral colourspaces at the moment.
#' @param interactive logical. If `TRUE`, uses the rgl engine for interactive plotting;
#' if `FALSE` then a static plot is generated.
#' @param col a vector of length 3 with the colours for (in order) the first volume,
#' the second volume, and the overlap.
#' @param fill logical. should the two volumes be filled in the plot? (defaults to `FALSE`)
#' @param new logical. Should a new plot window be called? If `FALSE`, volumes and their
#' overlap are plotted over the current plot (defaults to `TRUE`).
#' @param lwd if `plot = TRUE`, sets the line width for volume grids.
#' @param ... additional arguments passed to the plot. See [vol()]
#'
#' @return Calculates the overlap between the volumes defined by two set of points in
#' colourspace. The volume from the overlap is then given relative to:
#' - `vsmallest` the volume of the overlap divided by the smallest of that defined
#' by the the two input sets of colour points. Thus, if one of the volumes is entirely
#' contained within the other, this overlap will be `vsmallest = 1`.
#' - `vboth` the volume of the overlap divided by the combined volume of both
#' input sets of colour points.
#' If `type = "alpha"`, If used, the output will be different:
#' - `s_in1, s_in2` the number of sampled points that fall within each of the volumes
#' individually.
#' - `s_inboth` the number of sampled points that fall within both volumes.
#' - `s_ineither` the number of points that fall within either of the volumes.
#' - `psmallest` the proportion of points that fall within both volumes divided by the
#'  number of points that fall within the smallest volume.
#' - `pboth` the proportion of points that fall within both volumes divided by the total
#'  number of points that fall within both volumes.

#' @note Stoddard & Stevens (2011) originally obtained the volume overlap through Monte Carlo
#' simulations of points within the range of the volumes, and obtaining the frequency of
#' simulated values that fall inside the volumes defined by both sets of colour points.
#'
#' @note Stoddard & Stevens (2011) also return the value of the overlap relative to one of
#' the volumes (in that case, the host species). However, for other applications
#' this value may not be what one expects to obtain if (1) the two
#' volumes differ considerably in size, or (2) one of the volumes is entirely contained
#' within the other. For this reason, we also report the volume relative to the union of
#' the two input volumes, which may be more adequate in most cases.
#'
#' @examples
#' data(sicalis)
#' tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
#' tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
#' tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")
#'
#' # Convex hull volume
#' voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "convex")
#' voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex", plot = TRUE)
#' voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex", plot = TRUE, col = seq_len(3))
#'
#' # Alpha-shape volume
#' if (require("alphashape3d")) {
#'   voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "alpha", avalue = 1)
#' }
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#'
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage color
#' in a tetrahedral color space: A phylogenetic analysis of new world buntings. The
#' American Naturalist, 171(6), 755-776.
#' @references Stoddard, M. C., & Stevens, M. (2011). Avian vision and the evolution of
#' egg color mimicry in the common cuckoo. Evolution, 65(7), 2004-2013.
#' @references Maia, R., White, T. E., (2018) Comparing colors using visual models.
#'  Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}
#' @references
#' Gruson H. (2020). Estimation of colour volumes as concave hypervolumes using
#' \eqn{\alpha}{alpha}-shapes. Methods in Ecology and Evolution, 11(8), 955-963
#' \doi{10.1111/2041-210X.13398}

voloverlap <- function(colsp1, colsp2, type = c("convex", "alpha"), avalue = "auto",
                       plot = FALSE, interactive = FALSE,
                       col = c("blue", "red", "darkgrey"), fill = FALSE,
                       new = TRUE, nsamp = 1000, psize = 0.001,
                       lwd = 1, ...) {
  type <- match.arg(type)

  if (type == "alpha") {
    res <- overlap3d(
      colsp1, colsp2, avalue, plot, interactive, col,
      fill, new, nsamp, psize, lwd, ...
    )
  } else {
    if (!all(missing(nsamp), missing(psize))) {
      warning(
        'nsamp and psize arguments are deprecated for type = "convex" and will',
        " be ignored.",
        call. = FALSE
      )
    }


    dat1 <- as.matrix(colsp1[, colnames(colsp1) %in% c("x", "y", "z")])

    dat2 <- as.matrix(colsp2[, colnames(colsp1) %in% c("x", "y", "z")])

    over <- intersectn(dat1, dat2)

    vol1 <- over$ch1$vol
    vol2 <- over$ch2$vol

    overlapVol <- over$ch$vol

    vsmallest <- overlapVol / min(vol1, vol2)

    vboth <- overlapVol / (vol1 + vol2 - overlapVol)

    res <- data.frame(vol1, vol2, overlapvol = overlapVol, vsmallest, vboth)

    ##############
    # PLOT BEGIN #
    ##############
    if (plot) {
      if (ncol(dat1) < 3 || ncol(dat2) < 3) {
        warning("plot argument only works for tetrahedral colourspaces at the moment.")
        return(res)
      }

      if (length(col) < 3) {
        col <- c(rep(col, 2)[seq_len(2)], "darkgrey")
      }

      Voverlap <- over$ch$p

      if (interactive) {
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

        tcsvol(colsp1, col = col[1], fill = fill)
        tcsvol(colsp2, col = col[2], fill = fill)

        if (!is.null(Voverlap)) {
          colnames(Voverlap) <- c("x", "y", "z")
          attr(Voverlap, "clrsp") <- "tcs"
          tcsvol(Voverlap, col = col[3], fill = TRUE)
        }
      } else {
        plotrange <- apply(rbind(dat1, dat2), 2, range)

        vol(colsp1,
          col = col[1], lwd = lwd, new = new, fill = fill,
          xlim = plotrange[, "x"], ylim = plotrange[, "y"],
          zlim = plotrange[, "z"], ...
        )
        vol(colsp2, col = col[2], lwd = lwd, fill = fill, new = FALSE)

        if (!is.null(Voverlap)) {
          colnames(Voverlap) <- c("x", "y", "z")
          attr(Voverlap, "clrsp") <- "tcs"
          vol(Voverlap, col = col[3], lwd = lwd, fill = TRUE, new = FALSE)
        }
      }
      ############
      # PLOT END #
      ############
    }
  }

  res
}
