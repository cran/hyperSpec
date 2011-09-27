\name{summary}
\alias{Summary,-method}
\alias{Summary}
\title{The functions...}
\description{The functions}
\details{\code{Summary,-method}: \code{all}, \code{any},

\code{sum}, \code{prod},

\code{min}, \code{max}, 

\code{range}, and

\code{is.na}

for \code{hyperSpec} objects.

All these functions work on the spectra matrix.

}
\docType{methods}
\alias{Summary,hyperSpec-method}
\alias{Summary}
\alias{all,hyperSpec-method}
\alias{any,hyperSpec-method}
\alias{sum,hyperSpec-method}
\alias{prod,hyperSpec-method}
\alias{min,hyperSpec-method}
\alias{max,hyperSpec-method}
\alias{range,hyperSpec-method}
\value{\code{Summary,-method}: \code{sum}, \code{prod}, \code{min}, \code{max}, and \code{range} return  a numeric,
\code{all}, \code{any}, and \code{is.na} a logical.

}
\seealso{\code{\link[base]{Summary}} for the base summary functions.\code{\link[base]{all.equal}} and \code{\link[base]{isTRUE}}}
\arguments{\item{\dots}{further objects}
\item{na.rm}{logical indicating whether missing values should be removed}
\item{digits}{integer stating the rounding precision}
}
\examples{
range (flu)

is.na (flu [,, 405 ~ 410]);
}
\alias{is.na,-method}
\alias{is.na}
\alias{is.na,hyperSpec-method}

