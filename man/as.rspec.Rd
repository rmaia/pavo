\name{as.rspec}
\alias{as.rspec}
\alias{is.rspec}
\title{Convert data to an rspec object}
\usage{
  as.rspec(object, whichwl = NULL, interp = TRUE,
    lim = NULL)

  is.rspec(object)
}
\arguments{
  \item{object}{(required) a data frame or matrix
  containing spectra to process}

  \item{whichwl}{specifies which column contains
  wavelengths. If NULL (default), function searches for
  column containing equally spaced numbers and sets it as
  wavelengths "wl". If no wavelengths are found or
  \code{whichwl} is not given, returns arbitrary index
  values}

  \item{interp}{whether to interpolate wavelengths in 1-nm
  bins (defaults to TRUE)}

  \item{lim}{vector specifying wavelength range to
  interpolate over (e.g., \code{c(300, 700)})}
}
\value{
  an object of class \code{rspec} for use in further
  \code{pavo} functions

  a logical value indicating whether the object is of class
  \code{rspec}
}
\description{
  Converts data frames or matrices containing spectral data
  to \code{rspec} object
}
\examples{
\dontrun{

# Generate some fake reflectance data
fakedat <- data.frame(refl1 = rnorm(401), refl2 = rnorm(401), wavelength = c(300:700))
head(fakedat)

# Determine if is rspec object
is.rspec(fakedat)

# Convert to rspec object
fakedat2 <- as.rspec(fakedat)
is.rspec(fakedat2)
head(fakedat2)}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}

