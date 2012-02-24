\docType{methods}
\name{Summary}
\alias{all,hyperSpec-method}
\alias{any,hyperSpec-method}
\alias{is.na}
\alias{is.na,hyperSpec-method}
\alias{max,hyperSpec-method}
\alias{min,hyperSpec-method}
\alias{prod,hyperSpec-method}
\alias{range,hyperSpec-method}
\alias{sum,hyperSpec-method}
\alias{Summary}
\alias{Summary,hyperSpec-method}
\title{The functions}
\usage{
  \S4method{Summary}{hyperSpec}(x, ..., na.rm = FALSE)

  \S4method{is.na}{hyperSpec}(x)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{\dots}{further objects}

  \item{na.rm}{logical indicating whether missing values
  should be removed}
}
\value{
  \code{sum}, \code{prod}, \code{min}, \code{max}, and
  \code{range} return a numeric, \code{all}, \code{any},
  and \code{is.na} a logical.
}
\description{
  \code{all}, \code{any},
}
\details{
  \code{sum}, \code{prod},

  \code{min}, \code{max},

  \code{range}, and

  \code{is.na}

  for \code{hyperSpec} objects.

  All these functions work on the spectra matrix.
}
\examples{
range (flu)
is.na (flu [,, 405 ~ 410]);
}
\seealso{
  \code{\link[base]{Summary}} for the base summary
  functions.

  \code{\link[base]{all.equal}} and
  \code{\link[base]{isTRUE}}
}

