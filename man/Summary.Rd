\name{Summary}
\docType{methods}
\alias{Summary,hyperSpec-method}
\alias{hyperSpec Summary}
\alias{all,hyperSpec-method}
\alias{any,hyperSpec-method}
\alias{sum,hyperSpec-method}
\alias{prod,hyperSpec-method}
\alias{min,hyperSpec-method}
\alias{max,hyperSpec-method}
\alias{range,hyperSpec-method}
\title{Math Functions for hyperSpec Objects}
\description{The functions 

\code{all},
\code{any},

\code{sum},
\code{prod},

\code{min},
\code{max}, and

\code{range}

  for \code{hyperSpec} objects. 
}
\usage{
\S4method{all}{hyperSpec}(x, \dots, na.rm = FALSE)
\S4method{any}{hyperSpec}(x, \dots, na.rm = FALSE)
\S4method{sum}{hyperSpec}(x, \dots, na.rm = FALSE)
\S4method{prod}{hyperSpec}(x, \dots, na.rm = FALSE)
\S4method{min}{hyperSpec}(x, \dots, na.rm = FALSE)
\S4method{max}{hyperSpec}(x, \dots, na.rm = FALSE)
\S4method{range}{hyperSpec}(x, \dots, na.rm = FALSE)
}
\arguments{
  \item{x}{the \code{hyperSpec} object} 
  \item{\dots}{further objects} 
  \item{na.rm}{logical indicating whether missing values should be
          removed} 
}
\details{
  All these functions work on the spectra matrix. 
}

\value{a numeric}

\author{C. Beleites}

\examples{
	range (flu) 
}
\seealso{
  \code{\link[methods]{S4groupGeneric}} for group generic methods.

  \code{\link[base]{Summary}} for the base summary functions.
  
  \code{\link[hyperSpec]{Arith}} for arithmetic operators,
  \code{\link[hyperSpec]{Comparison}} for comparison operators, and 
  \code{\link[hyperSpec]{Math}}
  for group generic functions working on \code{hyperSpec} objects.  
}
\keyword{methods}
\keyword{arith}
