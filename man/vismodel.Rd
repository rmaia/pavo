\name{vismodel}
\alias{vismodel}
\title{Visual Models}
\usage{
  vismodel(specdata, sens = NULL, relative = TRUE,
    visual = c("avg.uv", "avg.v", "bt", "star", "pfowl"),
    luminance = c("bt.dc", "ch.dc", "ml", "none"),
    ilum = c("ideal", "bluesky", "D65", "forestshade"),
    bkg = "ideal")
}
\arguments{
  \item{specdata}{(required) Data frame containing
  reflectance spectra at each column. must contain a
  \code{wl} column identifying the wavelengths for the
  reflectance values.}

  \item{sens}{Data frame, such as one produced by
  \code{sensmodel} containing sensitivity for the
  user-defined visual system. The data frame must contain a
  'wl' column with the range of wavelengths included, and
  the sensitivity for each other cone as a column. If not
  specified, the visual system will be chosen according to
  one of the choices from the argument \code{visual}.}

  \item{visual}{The visual system to be used. Currently
  implemented system are: \code{avg.uv}: average avian UV
  system; \code{avg.v}: average avian V system; \code{bt}:
  Blue tit \emph{Cyanistes caeruleus} (REF); \code{star}:
  Starling \emph{Sturnus vulgaris} (REF); and \code{pfowl}:
  the peafowl \emph{Pavo cristatus} (REF).}

  \item{luminance}{The sensitivity data to be used to
  calculate luminance (achromatic) cone stimulation.
  Currently implemented system are: \code{bt.dc}: Blue tit
  \emph{Cyanistes caeruleus} double cone, \code{ch.dc}:
  Chicken \emph{Gallus gallus} double cone, \code{ml}: sum
  of the longest-wavelength cones, or \code{none}}

  \item{relative}{Should relative quantum catches be
  returned (i.e. is it a color space model? Defaults to
  \code{TRUE})}

  \item{ilum}{either a vector containing the illuminant, or
  one of the options: 'ideal' (total homogeneous iluminance
  accross wavelengths), 'bluesky', 'd65' (daylight), or
  'forestshade' (Default assumes an idealized iluminant of
  1)}

  \item{bkg}{either a vector containing the background
  spectra, or an ideal (white) background is used. in
  \code{specdata}. (Default assumes an idealized background
  of 1)}
}
\value{
  A list containing the following data frames:

  \code{descriptive}: descriptive statistics of maximum and
  normalized reflectance, and wavelength of maximum
  reflectance (hue)

  \code{Qi}: Quantum catch for each photoreceptor (which
  sum to 1 if \code{relative = TRUE})

  \code{qi}: Quantum catch normalized to the adapting
  background according to the von Kries transformation.

  \code{fi}: Quantum catch according to Fechner law (the
  signal of the receptor channel is proportional to the
  logarithm of the quantum catch)
}
\description{
  Applies the visual models of Vorobyev et al. (1998) to
  calculate quantum catches at each photoreceptor. Relative
  values may also be obtained, in which case the model
  reduces to the color space as described in Endler &
  Mielke (2005) and Stoddard & Prum (2008).
}
\examples{
\dontrun{
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual='avg.uv')
tcs.sicalis <- tcs(vis.sicalis, by=rep(c('C','T','B'),7))}
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Vorobyev, M., Osorio, D., Bennett, A., Marshall, N., &
  Cuthill, I. (1998). Tetrachromacy, oil droplets and bird
  plumage colours. Journal Of Comparative Physiology
  A-Neuroethology Sensory Neural And Behavioral Physiology,
  183(5), 621-633.

  Hart, N. S. (2001). The visual ecology of avian
  photoreceptors. Progress In Retinal And Eye Research,
  20(5), 675-703.

  Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian
  plumage color in a tetrahedral color space: A
  phylogenetic analysis of new world buntings. The American
  Naturalist, 171(6), 755-776.

  Endler, J. A., & Mielke, P. (2005). Comparing entire
  colour patterns as birds see them. Biological Journal Of
  The Linnean Society, 86(4), 405-431.
}

