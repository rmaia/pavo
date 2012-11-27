\name{procspec}
\alias{procspec}
\title{Process spectra}
\usage{
  procspec(specs,
    opt = c("none", "smooth", "maximum", "minimum", "stretch", "bin", "sum", "center"),
    fixneg = c("none", "addmin", "zero"), span = 0.25,
    bins = 20)
}
\arguments{
  \item{specs}{(required) an \code{rspec} object containing
  spectra to process}

  \item{select}{specification of which spectra to plot. Can
  be a numeric vector or factor (e.g., \code{sex=='male'})}

  \item{opt}{what type of processing options to apply. User
  can select multiple options by providing a vector.
  Possibilites are: \itemize{ \item \code{"none"} does not
  perform any processing (default).  \item \code{"smooth"}
  applies LOESS smoothing to each spectrum using
  \code{loess.smooth}.  \item \code{"minimum"} subtracts
  the minimum from each individual spectra.  \item
  \code{"maxmimum"} divides each spectrum by its maximum
  value \item \code{"sum"} divides each spectrum by summed
  values.  \item \code{"bin"} bins each spectrum into
  specified wavelength ranges. User should specify.  \item
  \code{"center"} centers individual spectra by subtracting
  mean reflectance from all values. }}

  \item{fixneg}{how to handle negative values.
  Possibilities are: \itemize{ \item \code{"none"} does not
  perform negative value correction (default).  \item
  \code{"zero"} sets all negative values to zero.  \item
  \code{"addmin"} adds the absolute value of the maximally
  negative values of each spectra to the reflectance at all
  other wavelengths (setting the minimum value to zero, but
  scaling other values accordingly). }}

  \item{span}{sets the smoothing parameter used by
  \code{loess.smooth}}

  \item{bins}{sets the number of equally sized wavelength
  bins for \code{opt="bin"}}
}
\description{
  Applies normalization and/or smoothing to spectra for
  further analysis or plotting
}
\examples{
\dontrun{
#INCLUDE EXAMPLE}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}
\references{
  Cuthill, I., Bennett, A. T. D., Partridge, J. & Maier, E.
  1999. Plumage reflectance and the objective assessment of
  avian sexual dichromatism. The American Naturalist, 153,
  183-200.

  Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and
  McGraw, K.J., eds. Bird Coloration. Volume 1 Mechanisms
  and measuremements. Harvard University Press, Cambridge,
  Massachusetts.
}
\seealso{
  \code{\link{loess.smooth}}
}

