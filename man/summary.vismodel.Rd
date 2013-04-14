\name{summary.vismodel}
\alias{summary.vismodel}
\title{Visual Model Summary}
\usage{
  \method{summary}{vismodel} (object, ...)
}
\arguments{
  \item{object}{(required) Results of \code{vismodel}}

  \item{...}{class consistency (ignored)}
}
\value{
  Returns all attributes chosen when calculating the visual
  model, as well as the default \code{data.frame} summary
}
\description{
  Returns the attributes used when calculating a visual
  model using \code{vismodel}
}
\examples{
\dontrun{
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual='avg.uv')
summary(vis.sicalis)
}
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

