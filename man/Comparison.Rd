\name{Compare}
\docType{methods}
\alias{hyperSpec Comparison}
\alias{hyperSpec compare}
\alias{hyperSpec Comparison Operators}
\alias{Compare,hyperSpec-method}
\alias{Compare,hyperSpec,hyperSpec-method}
\alias{<,hyperSpec,hyperSpec-method}
\alias{>,hyperSpec,hyperSpec-method}
\alias{<=,hyperSpec,hyperSpec-method}
\alias{>=,hyperSpec,hyperSpec-method}
\alias{==,hyperSpec,hyperSpec-method}
\alias{!=,hyperSpec,hyperSpec-method}
\alias{Compare,hyperSpec,matrix-method} 
\alias{Compare,hyperSpec,numeric-method}
\alias{Compare,matrix,hyperSpec-method}
\alias{Compare,numeric,hyperSpec-method}
\alias{all.equal,hyperSpec,hyperSpec-method}
\title{Comparison Operators: <, >, <=, >=, ==, and !=}
\description{The comparison operators \code{>}, \code{<},
  \code{>=}, \code{<=}, \code{==}, and \code{!=} 
  for \code{hyperSpec} objects. 
  
  \code{all.equal} checks the equality of two hyperSpec objects.}
\section{Usage}{
\code{#S4 method for signature 'hyperSpec,hyperSpec':}

\code{#S4 method for signature 'hyperSpec,matrix':}

\code{#S4 method for signature 'matrix,hyperSpec':}

\code{#S4 method for signature 'hyperSpec,numeric':}

\code{#S4 method for signature 'numeric,hyperSpec':}

\code{e1 > e2}

\code{`>` (e1, e2)}

\code{e1 < e2}

\code{e1 >= e2}

\code{e1 <= e2}

\code{e1 == e2}

\code{e1 != e2}

\code{all.equal(target, current, ..., check.column.order = FALSE, check.label = FALSE, check.log = FALSE)}
}
\arguments{
  \item{e1, e2}{Either two \code{hyperSpec} objects or one
	 \code{hyperSpec} object and  matrix of same size
    as \code{hyperSpec[[]]} or a scalar (numeric of length 1).
  } 
  \item{target, current}{two \code{hyperSpec} objects that are tested for equality}
  \item{\dots}{handed to \code{all.equal} when testing the slots of the \code{hyperSpec} objects}
  \item{check.column.order}{If two objects have the same data, but the order of the columns 
                            (determined by the names) differs, should they be regarded as different?}
  \item{check.label}{Should the slot \code{label} be checked? \cr
                     If the labels differ only in the order of their entries, they are conidered equal.}
  \item{check.log}{Should the slot \code{label} be checked?}
}
\details{
   The comparison operators \code{>}, \code{<},
  \code{>=}, \code{<=}, \code{==}, and \code{!=} work on the 
  spectra matrix of the \code{hyperSpec} object. They have their
  usual meaning (see \code{\link[base]{Comparison}}).
  The operators work also with one \code{hyperSpec} object and a numeric (scalar) object
  or a matrices of the same size as the spectra matrix of the \code{hyperSpec}
  object.

  With numeric vectors \code{\link[hyperSpec]{sweep}} might be more appropriate.
  
  If you want to calculate on the \code{data.frame} \code{hyperSpec@data}, you
  have to do this directly on \code{hyperSpec@data}.
}

\value{a logical matrix for the comparison operators.

As \code{hyperSpec} objects must have numeric spectra matrices, 
the resulting matrix of the comparison is returned directly.

\code{all.equal} returns either \code{TRUE}, or a character vector describing the differences.
In conditions, the result must therefore be tested with \code{\link[base]{isTRUE}}.
}

\author{C. Beleites}

\examples{
flu [,,445 ~ 450] > 300

all (flu == flu[[]])
}
\seealso{
  \code{\link[hyperSpec]{sweep-methods}} for calculations involving a vector and
  the spectral matrix.
  
  \code{\link[methods]{S4groupGeneric}} for group generic methods.

  \code{\link[base]{Comparison}} for the base comparison functions.
  
  \code{\link[hyperSpec]{Arith}} for arithmetic operators, 
  \code{\link[hyperSpec]{Math}}  for mathematical group generic 
  functions (groups Math and Math2) working on \code{hyperSpec} objects.  
  
  \code{\link[base]{all.equal}} and \code{\link[base]{isTRUE}}
}
\keyword{methods}
\keyword{arith}
