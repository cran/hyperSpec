\name{wl.eval}
\alias{wl.eval}
\title{Evaluate function on wavelengths of hyperSpec object}
\usage{
  wl.eval(x, ..., normalize.wl = I)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{\dots}{hyperSpec method: expressions to be
  evaluated}

  \item{normalize.wl}{function to transorm the wavelengths
  before evaluating the polynomial (or other function). Use
  \code{\link[hyperSpec]{normalize01}} to map the
  wavelength range to the interval [0, 1].}
}
\value{
  hyperSpec object containing one spectrum for each
  expression
}
\description{
  This is useful for generating certain types of baseline
  "reference spectra".
}
\examples{
plot (wl.eval (laser, exp = function (x) exp (-x)))
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[hyperSpec]{vanderMonde}} for polynomials,

  \code{\link[hyperSpec]{normalize01}} to normalize the
  wavnumbers before evaluating the function
}

