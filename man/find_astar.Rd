% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_astar.R
\name{find_astar}
\alias{find_astar}
\title{Compute the \eqn{\alpha^*}{alpha*} value}
\usage{
find_astar(coords)
}
\arguments{
\item{coords}{coordinates of the points for which you want to compute the
alphashape}
}
\value{
The \eqn{\alpha^*}{alpha*} value
as defined in Gruson (2020).
}
\description{
Compute the \eqn{\alpha^*}{alpha*} value
}
\references{
Gruson H. (2020). Estimation of colour volumes as concave hypervolumes using
\eqn{\alpha}{alpha}-shapes. Methods in Ecology and Evolution, 11(8), 955-963
\doi{10.1111/2041-210X.13398}
}
\seealso{
\code{\link[alphashape3d:ashape3d]{alphashape3d::ashape3d()}}
}
\keyword{internal}
