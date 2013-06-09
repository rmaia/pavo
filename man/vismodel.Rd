\name{vismodel}
\alias{vismodel}
\title{Visual Models}
\usage{
  vismodel(rspecdata, qcatch = c("Qi", "fi"),
    visual = c("avg.uv", "avg.v", "bt", "star", "pfowl"),
    achromatic = c("bt.dc", "ch.dc", "st.dc", "ml", "none"),
    illum = c("ideal", "bluesky", "D65", "forestshade"),
    vonkries = F, scale = 1, bkg = "ideal",
    relative = TRUE)
}
\arguments{
  \item{rspecdata}{(required) a data frame, possibly an
  object of class \code{rspec} that has wavelength range in
  the first column, named 'wl', and spectral measurements
  in the remaining columns.}

  \item{qcatch}{Which quantal catch metric to return.
  Options are: \itemize{ \item \code{Qi}: Quantum catch for
  each photoreceptor \item \code{fi}: Quantum catch
  according to Fechner law (the signal of the receptor
  channel is proportional to the logarithm of the quantum
  catch) }}

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
  double cone \item \code{st.dc}: Starling \emph{Sturnus
  vulgaris} double cone \item \code{ml}: sum of the two
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

  \item{vonkries}{logical. Should the von Kries color
  correction transformation be applied? (defaults to
  \code{FALSE})}

  \item{scale}{a value by which the illuminant will be
  multiplied. Useful for when the illuminant is a relative
  value (i.e. transformed to a maximum of 1 or to a
  percentage), and does not correspond to quantum flux
  units ($umol*s^-1*m^-2$). Useful values are, for example,
  500 (for dim light) and 10000 (for bright illumination).
  Note that if \code{vonkries=TRUE} this transformation has
  no effect.}
}
\value{
  An object of class \code{vismodel} containing the photon
  catches for each of the photoreceptors considered.
  Information on the parameters used in the calculation are
  also stored and can be called using the
  \code{summary.vismodel} function.
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
tcs.sicalis <- tcs(vis.sicalis)}
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

