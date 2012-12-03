\name{vismodel}
\alias{vismodel}
\title{Visual Models}
\usage{
  vismodel(rspecdata,
    visual = c("avg.uv", "avg.v", "bt", "star", "pfowl"),
    achromatic = c("bt.dc", "ch.dc", "ml", "none"),
    illum = c("ideal", "bluesky", "D65", "forestshade"),
    bkg = "ideal", relative = TRUE)
}
\arguments{
  \item{rspecdata}{(required) a data frame, possibly an
  object of class \code{rspec} that has wavelength range in
  the first column, named 'wl', and spectral measurements
  in the remaining columns.}

  \item{visual}{the visual system to be used. Options are:
  \itemize{ \item a data frame such as one produced
  containing by \code{sensmodel}, containing sensitivity
  for the user-defined visual system. The data frame must
  contain a \code{'wl'} column with the range of
  wavelengths included, and the sensitivity for each other
  cone as a column \item \code{avg.uv}: average avian UV
  system \item \code{avg.v}: average avian V system \item
  \code{bt}: Blue tit \emph{Cyanistes caeruleus} visual
  system \item \code{star}: Starling \emph{Sturnus
  vulgaris} visual system \item \code{pfowl}: Peafowl
  \emph{Pavo cristatus} visual system }}

  \item{achromatic}{the sensitivity data to be used to
  calculate luminance (achromatic) cone stimulation.
  Currently implemented options are: \itemize{ \item
  \code{bt.dc}: Blue tit \emph{Cyanistes caeruleus} double
  cone \item \code{ch.dc}: Chicken \emph{Gallus gallus}
  double cone \item \code{ml}: sum of the two
  longest-wavelength cones \item \code{none} }}

  \item{illum}{either a vector containing the illuminant,
  or one of the options: \itemize{ \item \code{ideal}:
  homogeneous illuminance of 1 accross wavelengths
  (default) \item \code{'bluesky'} \item \code{'D65'}:
  standard daylight \item \code{'forestshade'} }}

  \item{bkg}{either a vector containing the background
  spectra, or an ideal (white) background is used (Default
  assumes an idealized homogeneous background).}

  \item{relative}{should relative quantum catches be
  returned (i.e. is it a color space model? Defaults to
  \code{TRUE}).}
}
\value{
  A list containing the following data frames:

  \code{descriptive}: Descriptive statistics of maximum and
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

