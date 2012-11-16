\name{spec2rgb}
\alias{spec2rgb}
\title{Spectrum to rgb color conversion}
\usage{
  spec2rgb(specs)
}
\arguments{
  \item{specs}{(required) An rspec object of spectral data
  with columns as spectra.}
}
\value{
  a character vector of class \code{spec2rgb} consisting of
  hexadecimal color values for passing to further plotting
  functions.
}
\description{
  Calculates rgb values from spectra based on human color
  matching functions
}
\examples{
\dontrun{
#INCLUDE EXAMPLE}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}
\references{
  CIE. (1932). Commission Internationale de l’Éclairage
  Proceedings, 1931. Cambridge: Cambridge University Press.

  Color matching functions obtained from Colour and Vision
  Research Laboratory online data respository at
  http://www.cvrl.org/.

  http://www.cs.rit.edu/~ncs/color/t_spectr.html
}

