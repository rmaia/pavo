#' Plot spectra in a colourspace
#'
#' Plots reflectance spectra in the appropriate colourspace.
#'
#' @param x (required) an object of class `colspace`.
#' @param ... additional graphical options, which vary by modeled `space`. Refer
#'  to their individual documentation:
#' * [diplot()]: dichromat space
#' * [triplot()]: trichromat space
#' * [tetraplot()]: tetrahedral space
#' * [catplot()]: categorical space
#' * [hexplot()]: colour hexagon
#' * [cocplot()]: colour-opponent-coding space
#' * [cieplot()]: cie spaces
#' * [segplot()]: segment analysis space
#' * [jndplot()]: perceptual, 'noise corrected' space (for the results of [jnd2xyz()])
#'
#' Also see [par()].
#'
#' @return A colourspace plot appropriate to the input data.
#'
#' @examples
#' data(flowers)
#' data(sicalis)
#'
#' # Dichromat
#' vis.flowers <- vismodel(flowers, visual = "canis")
#' di.flowers <- colspace(vis.flowers, space = "di")
#' plot(di.flowers)
#'
#' # Colour hexagon
#' vis.flowers <- vismodel(flowers,
#'   visual = "apis", qcatch = "Ei", relative = FALSE,
#'   vonkries = TRUE, achromatic = "l", bkg = "green"
#' )
#' hex.flowers <- colspace(vis.flowers, space = "hexagon")
#' plot(hex.flowers, sectors = "coarse")
#'
#' # Tetrahedron (static)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' plot(tcs.sicalis)
#'
#' @examplesIf interactive() | identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Tetrahedron (interactive)
#' vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
#' tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
#' tcsplot(tcs.sicalis, size = 0.005)
#'
#' ## Add points to interactive tetrahedron
#' patch <- rep(c("C", "T", "B"), 7)
#' tcs.crown <- subset(tcs.sicalis, "C")
#' tcs.breast <- subset(tcs.sicalis, "B")
#' tcsplot(tcs.crown, col = "blue")
#' tcspoints(tcs.breast, col = "red")
#'
#' ## Plot convex hull in interactive tetrahedron
#' tcsplot(tcs.sicalis, col = "blue", size = 0.005)
#' tcsvol(tcs.sicalis)
#'
#' @seealso [plot()], [points.colspace()]
#'
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Thomas White \email{thomas.white026@@gmail.com}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#'
#' @importFrom graphics par
#'
#' @export
#'
#' @inherit colspace references


plot.colspace <- function(x, ...) {
  if ("jnd2xyz" %in% attr(x, "class")) {
    jndplot(x, ...)
  } else {
    space <- attr(x, "clrsp")

    switch(space,
      "dispace" = diplot(x, ...),
      "trispace" = triplot(x, ...),
      "hexagon" = hexplot(x, ...),
      "tcs" = tetraplot(x, ...),
      "coc" = cocplot(x, ...),
      "categorical" = catplot(x, ...),
      "CIEXYZ" = cieplot(x, ...),
      "CIELAB" = cieplot(x, ...),
      "CIELCh" = cieplot(x, ...),
      "segment" = segplot(x, ...)
    )
  }
}
