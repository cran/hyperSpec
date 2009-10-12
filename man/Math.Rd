\name{Math}
\docType{methods}
\alias{hyperSpec Math}
\alias{hyperSpec Math2}
\alias{Math,hyperSpec-method}
\alias{Math2,hyperSpec-method}
\alias{abs,hyperSpec-method}
\alias{sign,hyperSpec-method}
\alias{sqrt,hyperSpec-method}
\alias{floor,hyperSpec-method}
\alias{ceiling,hyperSpec-method}
\alias{trunc,hyperSpec-method}
\alias{round,hyperSpec-method}
\alias{signif,hyperSpec-method}
\alias{exp,hyperSpec-method}
\alias{log,hyperSpec-method}
\alias{expm1,hyperSpec-method}
\alias{log1p,hyperSpec-method}
\alias{cos,hyperSpec-method}
\alias{sin,hyperSpec-method}
\alias{tan,hyperSpec-method}
\alias{acos,hyperSpec-method}
\alias{asin,hyperSpec-method}
\alias{atan,hyperSpec-method}
\alias{cosh,hyperSpec-method}
\alias{sinh,hyperSpec-method}
\alias{tanh,hyperSpec-method}
\alias{acosh,hyperSpec-method}
\alias{asinh,hyperSpec-method}
\alias{atanh,hyperSpec-method}
\alias{lgamma,hyperSpec-method}
\alias{gamma,hyperSpec-method}
\alias{digamma,hyperSpec-method}
\alias{trigamma,hyperSpec-method}
\alias{cumsum,hyperSpec-method}
\alias{cumprod,hyperSpec-method}
\alias{cummax,hyperSpec-method}
\alias{cummin,hyperSpec-method}
\alias{round,hyperSpec-method}
\alias{signif,hyperSpec-method}
\title{Math Functions for hyperSpec Objects}
\description{The functions 
\code{abs},
\code{sign},

\code{sqrt},

\code{floor},
\code{ceiling},
\code{trunc},
\code{round},
\code{signif},

\code{exp},
\code{log},
\code{expm1},
\code{log1p},

\code{cos},
\code{sin},
\code{tan},
\code{acos},
\code{asin},
\code{atan},
\code{cosh},
\code{sinh},
\code{tanh},
\code{acosh},
\code{asinh},
\code{atanh},

\code{lgamma},
\code{gamma},
\code{digamma},
\code{trigamma},

\code{cumsum},
\code{cumprod},
\code{cummax},
\code{cummin}

  for \code{hyperSpec} objects. 
}
\section{Usage}{
\code{## S4 methods for signature 'hyperSpec':}

\code{abs (x)},
\code{sign (x)},

\code{sqrt (x)},

\code{floor (x)},
\code{ceiling (x)},
\code{trunc (x)},
\code{round (x, digits)},
\code{signif (x, digits)},

\code{exp (x)},
\code{log (x)},
\code{expm1 (x)},
\code{log1p (x)},

\code{cos (x)},
\code{sin (x)},
\code{tan (x)},
\code{acos (x)},
\code{asin (x)},
\code{atan (x)},
\code{cosh (x)},
\code{sinh (x)},
\code{tanh (x)},
\code{acosh (x)},
\code{asinh (x)},
\code{atanh (x)},

\code{lgamma (x)},
\code{gamma (x)},
\code{digamma (x)},
\code{trigamma (x)},

\code{cumsum (x)},
\code{cumprod (x)},
\code{cummax (x)},
\code{cummin (x)}
}
\arguments{
  \item{x}{the \code{hyperSpec} object} 
  \item{digits}{integer stating the rounding precision} 
}
\details{
  All these functions work on the spectra matrix. 
}

\value{a \code{hyperSpec} object}

\author{C. Beleites}

\examples{
	log (flu) 
}
\seealso{
  \code{\link[methods]{S4groupGeneric}} for group generic methods.

  \code{\link[base]{Math}} for the base math functions.
  
  \code{\link[hyperSpec]{Arith}} for arithmetic operators,
  \code{\link[hyperSpec]{Comparison}} for comparison operators, and 
  \code{\link[hyperSpec]{Summary}} 
    for group generic functions working on \code{hyperSpec} objects.  
}
\keyword{methods}
\keyword{math}
