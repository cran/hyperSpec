\name{vanderMonde}
\alias{vanderMonde}
\alias{vanderMonde,hyperSpec-method}
\title{Function evaluation on hyperSpec objects}
\usage{
  vanderMonde(x, order, ...)

  \S4method{vanderMonde}{hyperSpec}(x, order, ...,
    normalize.wl = normalize01)
}
\arguments{
  \item{x}{vector with values to evaluate the polynomial on

  hyperSpec object}

  \item{order}{of the polynomial}

  \item{normalize.wl}{function to transorm the wavelengths
  before evaluating the polynomial (or other function).
  \code{\link[hyperSpec]{normalize01}} maps the wavelength
  range to the interval [0, 1]. Use \code{\link[base]{I}}
  to turn off.}

  \item{\dots}{hyperSpec method: further arguments to
  \code{\link{decomposition}}}
}
\value{
  van der Monde matrix

  hyperSpec method: hyperSpec object containing van der
  Monde matrix as spectra and an additional column
  ".vdm.order" giving the order of each spectrum (term).
}
\description{
  vandermonde generates van der Monde matrices, the
  hyperSpec method generates a hyperSpec object containing
  the van der Monde matrix of the wavelengths of a
  hyperSpec object.
}
\details{
  It is often numerically preferrable to map \code{wl (x)}
  to [0, 1], see the example.
}
\examples{
plot (vanderMonde (flu, 2))
plot (vanderMonde (flu, 2, normalize.wl = I))
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[hyperSpec]{wl.eval}} for calculating
  arbitrary functions of the wavelength,

  \code{\link[hyperSpec]{normalize01}}
}

