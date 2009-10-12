\name{as.data.frame-methods}
\docType{methods}
\alias{as.data.frame}
\alias{as.long.df}
\alias{as.data.frame-methods}
\alias{as.data.frame,hyperSpec,missing,missing-method}
\alias{as.data.frame,hyperSpec-method}
\alias{as.matrix}
\alias{as.matrix-methods}
\alias{as.matrix,ANY-method}
\alias{as.matrix,hyperSpec-method}
\title{Conversion of a hyperSpec object into a data.frame or matrix}
\description{
  \code{as.data.frame} returns \code{x@data}
  \code{as.long.df} returns a long-format data.frame

  \code{as.matrix} returns the spectra matrix \code{x@data$spc}
}
\usage{
\S4method{as.data.frame}{hyperSpec,missing,missing}(x, row.names = NULL, optional = FALSE, \dots)
as.long.df (x, rownames = FALSE)
\S4method{as.matrix}{hyperSpec}(x, \dots)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
  \item{row.names, optional, \dots}{ignored}
  \item{rownames}{should the rownames be in column \code{.rownames} of
	 the long-format data.frame?}
}
\value{
  \code{x@data} and \code{x@data$spc}, respectively.
}
\details{
  Long-format data.frame: this is the stacked or molten version of \code{x@data}.
}

\author{C. Beleites}
\seealso{
  \code{\link[base]{as.data.frame}} and \code{\link[base]{as.matrix}}

  \code{\link[hyperSpec:extract_replace]{[}}

  \code{\link[utils]{stack}} and \code{\link[reshape]{melt}} for other
  functions producing long-format data.frames.
}
\examples{
as.data.frame (chondro [1:3,, 600:620])
as.matrix (chondro [1:3,, 600:620])
}

\keyword{methods}

