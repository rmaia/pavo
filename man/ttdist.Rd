\name{ttdist}
\alias{ttdist}
\title{Tetrachromatic color distances}
\usage{
  ttdist(vismodeldata, qcatch = c("Qi", "qi", "fi"),
    n1 = 1, n2 = 2, n3 = 2, n4 = 4, v = 0.1,
    asdist = FALSE)
}
\arguments{
  \item{vismodeldata}{(required) Quantum catch color data.
  Can be either the result from \code{vismodel} or
  independently calculated data (in the form of a data
  frame with four columns, representing the avian cones).}

  \item{qcatch}{Quantum catch values to use in the model.
  Can be either \code{Qi}, \code{qi} or \code{fi} (defaults
  to \code{Qi}).}

  \item{n1,n2,n3,n4}{Photoreceptor densities for u, s, m &
  l (default to blue tit densities: 1,2,2,4)}

  \item{v}{Noise-to-signal ratio of a single cone (defaults
  to 0.1, so that under the default densities, the Weber
  fraction for the large cone will be 0.05, as estimated
  from beharioal experiment with the Perkin robin,
  \emph{Leiothrix lutea})}

  \item{asdist}{if \code{TRUE}, returns result in the form
  of a distance matrix; if \code{FALSE} returns as a data
  frame (see below; defaults to \code{FALSE})}
}
\value{
  Either a symmetrical distance matrix based on pairs of
  patches, or a data frame containing 3 columns (patch1,
  patch2, and the dS distance)
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
ttd.sicalis <- ttdist(vis.sicalis, qcatch='fi', asdist=T)}
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
  The Linnean Society, 86(4), 405–431.
}

