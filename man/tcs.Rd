\name{tcs}
\alias{tcs}
\title{Tetracolorspace avian visual model}
\usage{
  tcs(vismodeldata, by = NULL,
    qcatch = c("Qi", "qi", "fi"))
}
\arguments{
  \item{vismodeldata}{(required) Quantum catch color data.
  Can be either the result from \code{vismodel} or
  independently calculated data (in the form of a data
  frame with four columns, representing the avian cones).}

  \item{by}{Either a single value specifying the range of
  color points for which summary colorspace variables
  should be calculated (for example, \code{by} = 3
  indicates summary will be calculated for groups of 3
  consecutive color points (rows) in the quantum catch
  color data frame) or a vector containing identifications
  for the rows in the quantum catch color data frame (in
  which case summaries will be calculated for each group of
  points sharing the same identification). If \code{by} is
  left blank, the summary statistics are calculated accross
  all color points in the data.}

  \item{qcatch}{Quantum catch values to use in the model.
  Can be either \code{Qi}, \code{qi} or \code{fi} (defaults
  to \code{Qi}).}
}
\value{
  a data frame consisting of the following rows:

  \code{u}, \code{s}, \code{m}, \code{l}: the quantum catch
  data used to calculate the remaining variables. NOTE:
  even if visual sistem is of type V-VIS, the output column
  will be labeled \code{u}.

  \code{u.r}, \code{s.r}, \code{m.r}, \code{l.r}: relative
  cone stimulation, for a given hue, as a function of
  saturation. See Stoddard & Prum (2008) for details.

  \code{x}, \code{y}, \code{z}: cartesian coordinates for
  the points in the tetrahedral color space.

  \code{h.theta}, \code{h.phi}: angles theta and phi, in
  radians, determining the hue of the color.

  \code{r.vec}: the r vector (saturation, distance from the
  achromatic center).

  \code{r.max}: the maximum r vector achievable for the
  color's hue.

  \code{r.achieved}: the relative r distance from the
  achromatic center, in relation to the maximum distance
  achievable (\code{r.vec/r.max})
}
\description{
  calculates coordinates and colorimetric variables that
  represent reflectance spectra in the avian tetrahedral
  color space.
}
\examples{
\dontrun{
#INCLUDE EXAMPLE}
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian
  plumage color in a tetrahedral color space: A
  phylogenetic analysis of new world buntings. The American
  Naturalist, 171(6), 755-776.

  Endler, J. A., & Mielke, P. (2005). Comparing entire
  colour patterns as birds see them. Biological Journal Of
  The Linnean Society, 86(4), 405–431.
}

