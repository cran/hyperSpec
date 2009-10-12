\name{Arith}
\docType{methods}
\alias{hyperSpec Arith}
\concept{hyperSpec arithmetic}
\concept{hyperSpec arithmetical operators}
\concept{hyperSpec matrix multiplication}
\concept{hyperSpec plus}
\concept{hyperSpec division}
\concept{hyperSpec spectra conversion}
\alias{Arith-method}
\alias{Arith,hyperSpec-method}
\alias{Arith,hyperSpec,hyperSpec-method}
\alias{+,hyperSpec,hyperSpec-method}
\alias{-,hyperSpec,hyperSpec-method}
\alias{*,hyperSpec,hyperSpec-method}
\alias{^,hyperSpec,hyperSpec-method}
\alias{\%\%,hyperSpec,hyperSpec-method}
\alias{\%/\%,hyperSpec,hyperSpec-method}
\alias{/,hyperSpec,hyperSpec-method}
\alias{Arith,hyperSpec,matrix-method} 
\alias{Arith,hyperSpec,numeric-method}
\alias{Arith,hyperSpec,missing-method}
\alias{Arith,matrix,hyperSpec-method}
\alias{Arith,numeric,hyperSpec-method}
\alias{\%*\%}
\alias{\%*\%,hyperSpec,hyperSpec-method}
\alias{\%*\%,matrix,hyperSpec-method}
\alias{\%*\%,hyperSpec,matrix-method}
\title{Arithmetical Operators: +, -, *, /, ^, \%\%, \%/\%, \%*\%}
\description{The arithmetical operators \code{+}, \code{-},
  \code{*}, \code{/}, \code{\^}, \code{\%\%}, \code{\%/\%},
  and \code{\%*\%} for \code{hyperSpec} objects. 
}
\usage{
\S4method{Arith}{hyperSpec,hyperSpec}(e1, e2)
\S4method{Arith}{hyperSpec,matrix}(e1, e2)
\S4method{Arith}{matrix,hyperSpec}(e1, e2)
\S4method{Arith}{hyperSpec,numeric}(e1, e2)
\S4method{Arith}{numeric,hyperSpec}(e1, e2)
\S4method{Arith}{hyperSpec,missing}(e1, e2)
x \%*\% y
}
\arguments{
  \item{e1, e2}{Either two \code{hyperSpec} objects or one
	 \code{hyperSpec} object and  matrix of same size
    as \code{hyperSpec[[]]} or a scalar (numeric of length 1).
  } 
  
  \item{x, y}{Either two \code{hyperSpec} objects or one
	 \code{hyperSpec} object and one matrix of appropriate size.
  }
}
\details{
  You can use these operators in different ways:
  \preformatted{
e1 + e2
`+` (e1, e2)

x \%*\% y
`\%*\%`(x, y)

-x
  }
  The arithmetical operators \code{+}, \code{-}, \code{*}, \code{/},
  \code{^}, \code{\%\%}, \code{\%/\%}, and \code{\%*\%} work on the 
  spectra matrix of the \code{hyperSpec} object. They have their
  usual meaning (see \code{\link[base]{Arithmetic}}).
  The operators work also with one \code{hyperSpec} object and a numeric object
  or a matrices of the same size as the spectra matrix of the \code{hyperSpec}
  object.

  With numeric vectors \code{\link[hyperSpec]{sweep}} is most probably more appropriate.
  
  If you want to calculate on the \code{data.frame} \code{hyperSpec@data}, you
  have to do this directly on \code{hyperSpec@data}.
}

\value{\code{hyperSpec} object with the new spectra matrix.}

\author{C. Beleites}

\examples{
chondro + chondro
1 / chondro
all((chondro + chondro - 2 * chondro)[[]] == 0)
-flu
}
\seealso{
  \code{\link[hyperSpec]{sweep-methods}} for calculations involving a vector and
  the spectral matrix.
  
  \code{\link[methods]{S4groupGeneric}} for group generic methods.

  \code{\link[base]{Arithmetic}} for the base arithmetic functions.

  \code{\link[base]{matmult}} for matrix multiplications with \code{\%*\%}.
  
  \code{\link[hyperSpec]{Comparison}} for comparison operators, 
  \code{\link[hyperSpec]{Math}} for mathematical group generic 
  functions (Math and Math2 groups) working on \code{hyperSpec} objects.  
}
\keyword{methods}
\keyword{arith}
