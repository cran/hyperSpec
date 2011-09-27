\name{dim}
\alias{ncol,hyperSpec-method}
\alias{ncol}
\title{The Number of Rows (Spectra), Columns, and Data Points per Spectrum of an...}
\usage{\S4method{ncol}{hyperSpec}(x)

\S4method{nrow}{hyperSpec}(x)


nwl(x)


\S4method{dim}{hyperSpec}(x)


\S4method{length}{hyperSpec}(x)

}
\description{The Number of Rows (Spectra), Columns, and Data Points per Spectrum of an
hyperSpec Object)}
\details{\code{ncol,hyperSpec-method}: \code{ncol} returns the number of columns in \code{x@data}. I.e. the number
of columns with additional information to each spectrum (e.g. "x", "y",
\dots{}) + 1 (for column \code{spc} containing the spectra).

}
\docType{methods}
\author{C. Beleites}
\seealso{\code{\link[base]{ncol}}\code{\link[base]{nrow}}\code{\link[base]{dim}}\code{\link[base]{length}}}
\value{\code{ncol,hyperSpec-method}: \code{nrow}, \code{ncol}, \code{nwl}, and \code{length}, return an
\code{integer}.

\code{dim,hyperSpec-method}: \code{dim} returns a vector of length 3.


}
\arguments{\item{x}{a \code{hyperSpec} object}
}
\examples{
ncol (chondro)

nrow (chondro)

nwl  (chondro)

dim (chondro)

length (chondro)
}
\alias{nrow,hyperSpec-method}
\alias{nrow}
\alias{nwl}
\alias{nwl}
\alias{dim,hyperSpec-method}
\alias{dim}
\keyword{methods}
\alias{length,hyperSpec-method}
\alias{length}

