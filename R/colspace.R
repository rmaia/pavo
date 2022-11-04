#' Model spectra in a colorspace
#'
#' Models reflectance spectra in a colorspace. For information on plotting
#' arguments and graphical parameters, see [plot.colspace()].
#'
#' @param vismodeldata (required) quantum catch color data. Can be either the
#'   result from [vismodel()] or independently calculated data (in the form of a
#'   data frame with columns representing quantum catches).
#' @param space Which colorspace/model to use. Options are:
#' * `auto`: if data is a result from [vismodel()], applies `di`, `tri` or `tcs`
#' if input visual model had two, three or four cones, respectively.
#' * `di`: dichromatic colourspace. See [dispace()] for details.
#' ([plotting arguments][diplot])
#' * `tri`: trichromatic colourspace (i.e. Maxwell triangle). See [trispace()]
#' for details. ([plotting arguments][triplot])
#' * `tcs`: tetrahedral colourspace. See [tcspace()] for details.
#' ([plotting arguments][tetraplot])
#' * `hexagon`: the trichromatic colour-hexagon of Chittka (1992). See
#' [hexagon()] for details. ([plotting arguments][hexplot])
#' * `coc`: the trichromatic colour-opponent-coding model of Backhaus (1991).
#' See [coc()] for details. ([plotting arguments][cocplot])
#' * `categorical`: the tetrachromatic categorical fly-model of Troje (1993).
#' See [categorical()] for details. ([plotting arguments][catplot])
#' * `ciexyz`: CIEXYZ space. See [cie()] for details.
#' ([plotting arguments][cieplot])
#' * `cielab`: CIELAB space. See [cie()] for details.
#' ([plotting arguments][cieplot])
#' * `cielch`: CIELCh space. See [cie()] for details.
#' ([plotting arguments][cieplot])
#' * `segment`: segment analysis of Endler (1990). See [segspace()] for details.
#' ([plotting arguments][segplot])
#' @param qcatch Which quantal catch metric is being inputted. Only used when
#'   input data is NOT an output from [vismodel()]. Must be `Qi`, `fi` or `Ei`.
#' @param ... additional arguments passed to [cie()] for non `vismodel()` data.
#'
#' @examples
#' data(flowers)
#'
#' # Model a dichromat viewer in a segment colourspace
#' vis.flowers <- vismodel(flowers, visual = "canis")
#' di.flowers <- colspace(vis.flowers, space = "di")
#'
#' # Model a honeybee viewer in the colour hexagon
#' vis.flowers <- vismodel(flowers,
#'   visual = "apis", qcatch = "Ei", relative = FALSE,
#'   vonkries = TRUE, achromatic = "l", bkg = "green"
#' )
#' hex.flowers <- colspace(vis.flowers, space = "hexagon")
#'
#' # Model a trichromat (the honeybee) in a Maxwell triangle
#' vis.flowers <- vismodel(flowers, visual = "apis")
#' tri.flowers <- colspace(vis.flowers, space = "tri")
#' plot(tri.flowers)
#'
#' # Model a tetrachromat (the Blue Tit) in a tetrahedral colourspace
#' vis.flowers <- vismodel(flowers, visual = "bluetit")
#' tcs.flowers <- colspace(vis.flowers, space = "tcs")
#'
#' # Model a housefly in the 'categorical' colourspace
#' vis.flowers <- vismodel(flowers, visual = "musca", achro = "md.r1")
#' cat.flowers <- colspace(vis.flowers, space = "categorical")
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#'
#' @export
#'
#' @references Smith T, Guild J. (1932) The CIE colorimetric standards and their
#'   use. Transactions of the Optical Society, 33(3), 73-134.
#' @references Westland S, Ripamonti C, Cheung V. (2012). Computational colour
#'   science using MATLAB. John Wiley & Sons.
#' @references Chittka L. (1992). The colour hexagon: a chromaticity diagram
#'   based on photoreceptor excitations as a generalized representation of
#'   colour opponency. Journal of Comparative Physiology A, 170(5), 533-543.
#' @references Chittka L, Shmida A, Troje N, Menzel R. (1994). Ultraviolet as a
#'   component of flower reflections, and the colour perception of Hymenoptera.
#'   Vision research, 34(11), 1489-1508.
#' @references Troje N. (1993). Spectral categories in the learning behaviour of
#'   blowflies. Zeitschrift fur Naturforschung C, 48, 96-96.
#' @references Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian plumage
#'   color in a tetrahedral color space: A phylogenetic analysis of new world
#'   buntings. The American Naturalist, 171(6), 755-776.
#' @references Endler, J. A., & Mielke, P. (2005). Comparing entire colour
#'   patterns as birds see them. Biological Journal Of The Linnean Society,
#'   86(4), 405-431.
#' @references Kelber A, Vorobyev M, Osorio D. (2003). Animal colour vision -
#'   behavioural tests and physiological concepts. Biological Reviews, 78, 81 -
#'   118.
#' @references Backhaus W. (1991). Color opponent coding in the visual system of
#'   the honeybee. Vision Research, 31, 1381-1397.
#' @references Endler, J. A. (1990) On the measurement and classification of
#'   color in studies of animal color patterns. Biological Journal of the
#'   Linnean Society, 41, 315-352.

colspace <- function(vismodeldata,
                     space = c("auto", "di", "tri", "tcs", "hexagon", "coc", "categorical", "ciexyz", "cielab", "cielch", "segment"),
                     qcatch = NULL,
                     ...) {
  space2 <- try(match.arg(space), silent = TRUE)

  if (inherits(space2, "try-error")) {
    stop("Invalid colorspace selected")
  }

  # Auto-define an appropriate space
  if (space2 == "auto") {
    if (all(c("X", "Y", "Z") %in% names(vismodeldata))) {
      space2 <- "ciexyz"
    } else {
      if (is.null(attr(vismodeldata, "conenumb"))) {
        stop(
          "conenumb attribute is missing from vismodeldata. Please check",
          " that is a valid vismodel object or manually specify the space",
          " argument"
        )
      }
      space2 <-
        switch(as.character(attr(vismodeldata, "conenumb")),
          "2" = "di",
          "3" = "tri",
          "4" = "tcs",
          "seg" = "segment"
        )
    }
  }

  # Run the model
  res <-
    switch(space2,
      "di" = dispace(vismodeldata),
      "tri" = trispace(vismodeldata),
      "hexagon" = hexagon(vismodeldata),
      "tcs" = tcspace(vismodeldata),
      "coc" = coc(vismodeldata),
      "categorical" = categorical(vismodeldata),
      "ciexyz" = cie(vismodeldata, "XYZ", ...),
      "cielab" = cie(vismodeldata, "LAB", ...),
      "cielch" = cie(vismodeldata, "LCh", ...),
      "segment" = segspace(vismodeldata)
    )

  # Include lum for appropriate spaces
  if (!any(space2 %in% c("segment", "ciexyz", "cielab", "cielch"))) {
    res$lum <- vismodeldata$lum
  }

  # check qcatch if user-defined input
  if (is.null(attr(res, "qcatch"))) {
    if (is.null(qcatch)) {
      qcatch <- "Qi"
      message(
        'Input is not a "vismodel" object and argument "qcatch" is undefined; ',
        'assuming quantum catch are not transformed (i.e. qcatch = "Qi")'
      )
    }
    attr(res, "qcatch") <- qcatch
  }

  # Check relative if user-defined input
  if (is.null(attr(res, "relative"))) {
    attr(res, "relative") <- FALSE
    receptcols <- res[, colnames(res) %in% c("u", "s", "m", "l")]
    if (isTRUE(all.equal(rowSums(receptcols), rowSums(receptcols / rowSums(receptcols)), tol = 0.001))) {
      attr(res, "relative") <- TRUE
    }
  }

  res
}
