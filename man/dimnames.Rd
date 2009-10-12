\name{dimnames}
\docType{methods}
\alias{dimnames,hyperSpec-method}
\alias{colnames,hyperSpec-method}
\alias{rownames,hyperSpec-method}
\alias{colnames<-,hyperSpec-method}
\alias{rownames<-,hyperSpec-method}

\title{Dimnames of an hyperSpec Object}
\description{
  \code{rownames} yields the names of the rows (spectra) of a \code{hyperSpec} object,
  
  \code{colnames} the names of the data colums, and 
  
  \code{dimnames} both together with the names of the wavelengths (see
  below).
}
\usage{
\S4method{colnames}{hyperSpec}(x, do.NULL = TRUE, prefix = "col")
\S4method{rownames}{hyperSpec}(x, do.NULL = TRUE, prefix = "row")
\S4method{dimnames}{hyperSpec}(x)
}
\details{
\code{dimnames} returns in element \code{wl} the colnames of the spectra
matrix, \emph{not} the values. 

The replacement functions for column and row names are used like:
\preformatted{
colnames (x) <- value
rownames (x) <- value
}

If changing the colnames, be careful not to touch \code{spc}. Otherwise
an error about an invalid hyperSpec object results.
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
  \item{do.NULL, prefix}{handed to \code{\link[base]{rownames}} and
	 \code{\link[base]{colnames}}, respectively.}
}
\value{
  \code{rownames} and \code{colnames} return an \code{character} vector,
  
   \code{dimames} returns a list with elements \code{row},  \code{data}, \code{wl} with the 
   respective character vectors. 
}
\author{C. Beleites}
\seealso{\code{\link[base]{rownames}}, \code{\link[base]{colnames}}, and
  \code{\link[base]{dimnames}}

  \code{\link[base]{rownames<-}} and \code{\link[base]{colnames<-}} for the
  replacement functions

\code{\link[hyperSpec]{wl}}, \code{\link[hyperSpec]{wl<-}} for
accessing the values of the wavelength axis. 
}
\examples{
rownames (flu)
colnames (flu)
dimnames (laser)

rownames (flu) <- paste ("sample", LETTERS [1 : 6])
flu$..
}
\keyword{methods}
