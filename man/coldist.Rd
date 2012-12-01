\name{coldist}
\alias{coldist}
\title{Color distances}
\usage{
  coldist(vismodeldata, qcatch = c("Qi", "qi", "fi"),
    vis = c("tetra", "tri", "di"), achro = TRUE, n1 = 1,
    n2 = 2, n3 = 2, n4 = 4, v = 0.1)
}
\arguments{
  \item{vismodeldata}{(required) quantum catch color data.
  Can be either the result from \code{\link{vismodel}} or
  independently calculated data (in the form of a data
  frame with four columns, representing the avian cones).}

  \item{qcatch}{quantum catch values to use in the model:
  \itemize{ \item \code{Qi}: Quantum catch for each
  photoreceptor (default) \item \code{qi}: Quantum catch
  normalized to the adapting background according to the
  von Kries transformation \item \code{fi}: Quantum catch
  according to Fechner law (the signal of the receptor
  channel is proportional to the logarithm of the quantum
  catch) }}

  \item{vis}{visual system phenotype to use in the model:
  \itemize{ \item \code{tetra}: Tetrachromatic color vision
  (default) \item \code{tri}: Trichromatic color vision
  \item \code{di}: Dichromatic color vision }}

  \item{achro}{logical. If \code{TRUE}, last column of the
  data frame is used to calculate the achromatic contrast,
  with noise based on the Weber fraction calculated using
  \code{n4}}

  \item{n1,n2,n3,n4}{tetrachromatic photoreceptor densities
  for u, s, m & l (default to blue tit \emph{Cyanistes
  caeruleus} densities: 1:2:2:4). If \code{vis} does not
  equal \code{'tetra'}, only \code{n1} and \code{n2}
  (\code{vis='di'}) or \code{n1}, \code{n2} and \code{n3}
  (\code{vis='tri'}) are used for chromatic contrast (NOTE:
  \code{n4} is still the value used for the achromatic
  contrast.)}

  \item{v}{Noise-to-signal ratio of a single cone (defaults
  to 0.1, so that under the default densities, the Weber
  fraction for the large cone will be 0.05, as estimated
  from behavioral experiment with the Perkin robin,
  \emph{Leiothrix lutea})}

  \item{asdist}{logical. If \code{TRUE}, returns result in
  the form of a distance matrix; if \code{FALSE} returns as
  a data frame (see below; defaults to \code{FALSE})}
}
\value{
  A data frame containing 4 columns. The first two
  (\code{patch1, patch2}) refer to the two colors being
  contrasted; \code{dS} is the chromatic contrast (delta S,
  in JNDs) and \code{dL} is the achromatic contrast (delta
  L, in JNDs)
}
\description{
  Applies the visual models of Vorobyev et al. (1998) to
  calculate color distances with receptor noise based on
  relative photoreceptor densities.
}
\examples{
\dontrun{
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual='avg.uv', relative=FALSE)
coldist.sicalis <- coldist(vis.sicalis, qcatch='fi', vis='tetra')}
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

  Endler, J. A., & Mielke, P. (2005). Comparing entire
  colour patterns as birds see them. Biological Journal Of
  The Linnean Society, 86(4), 405-431.
}

