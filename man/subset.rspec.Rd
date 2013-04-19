\name{subset.rspec}
\alias{subset.rspec}
\alias{subset.tcs}
\alias{subset.vismodel}
\title{Subset rspec, tcs and vismodel objects}
\usage{
  \method{subset}{rspec} (x, subset, ...)

  \method{subset}{tcs} (x, subset, ...)

  \method{subset}{vismodel} (x, subset, ...)
}
\arguments{
  \item{x}{(required) an object of class \code{rspec},
  \code{tcs} or \code{vismodel} containing spectra, visual
  model output or colourspace data to subset}

  \item{...}{class consistency (ignored).}

  \item{subset}{a string used for partial matching of
  observations}
}
\value{
  a subsetted object of the same class as the input object
}
\description{
  Subsets various object types based on a given vector or
  grep partial matching of data names
}
\examples{
\dontrun{

# Load the 'sicalis' dataset
data(sicalis)
# Generate a visual model
vm1 <- vismodel(sicalis)
# Make a tetracolorspace
tcs1 <- tcs(vm1)
# Subset all 'crown' patches (C in file names)
head(subset(sicalis, "B"))
subset(vm1, "B")
subset(tcs1, "B")[, 1:5]}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}

