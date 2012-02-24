\name{Math2}
\alias{abs,hyperSpec-method}
\alias{acosh,hyperSpec-method}
\alias{acos,hyperSpec-method}
\alias{asinh,hyperSpec-method}
\alias{asin,hyperSpec-method}
\alias{atanh,hyperSpec-method}
\alias{atan,hyperSpec-method}
\alias{ceiling,hyperSpec-method}
\alias{cosh,hyperSpec-method}
\alias{cos,hyperSpec-method}
\alias{cummax,hyperSpec-method}
\alias{cummin,hyperSpec-method}
\alias{cumprod,hyperSpec-method}
\alias{cumsum,hyperSpec-method}
\alias{digamma,hyperSpec-method}
\alias{exp,hyperSpec-method}
\alias{expm1,hyperSpec-method}
\alias{floor,hyperSpec-method}
\alias{gamma,hyperSpec-method}
\alias{lgamma,hyperSpec-method}
\alias{log}
\alias{log1p,hyperSpec-method}
\alias{log,hyperSpec-method}
\alias{Math}
\alias{Math2}
\alias{Math2,hyperSpec-method}
\alias{Math,hyperSpec-method}
\alias{round,hyperSpec-method}
\alias{sign,hyperSpec-method}
\alias{signif,hyperSpec-method}
\alias{sinh,hyperSpec-method}
\alias{sin,hyperSpec-method}
\alias{sqrt,hyperSpec-method}
\alias{tanh,hyperSpec-method}
\alias{tan,hyperSpec-method}
\alias{trigamma,hyperSpec-method}
\alias{trunc,hyperSpec-method}
\title{Math Functions for hyperSpec Objects}
\usage{
  \S4method{Math2}{hyperSpec}(x, digits)

  \S4method{log}{hyperSpec}(x, base = exp(1), ...,
    short = "log", user = NULL, date = NULL)

  \S4method{Math}{hyperSpec}(x)
}
\arguments{
  \item{x}{the \code{hyperSpec} object}

  \item{digits}{integer stating the rounding precision}

  \item{\dots}{ignored}

  \item{base}{base of logarithm}

  \item{short,user,date}{handed to
  \code{\link[hyperSpec]{logentry}}}
}
\value{
  a \code{hyperSpec} object
}
\description{
  The functions \code{abs}, \code{sign},
}
\details{
  \code{sqrt},

  \code{floor}, \code{ceiling}, \code{trunc}, \code{round},
  \code{signif},

  \code{exp}, \code{log}, \code{expm1}, \code{log1p},

  \code{cos}, \code{sin}, \code{tan}, \code{acos},
  \code{asin}, \code{atan}, \code{cosh}, \code{sinh},
  \code{tanh}, \code{acosh}, \code{asinh}, \code{atanh},

  \code{lgamma}, \code{gamma}, \code{digamma},
  \code{trigamma},

  \code{cumsum}, \code{cumprod}, \code{cummax},
  \code{cummin}

  for \code{hyperSpec} objects.
}
\examples{
log (flu)
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[methods]{S4groupGeneric}} for group generic
  methods.

  \code{\link[base]{Math}} for the base math functions.

  \code{\link[hyperSpec]{Arith}} for arithmetic operators,
  \code{\link[hyperSpec]{Comparison}} for comparison
  operators, and \code{\link[hyperSpec]{Summary}} for group
  generic functions working on \code{hyperSpec} objects.
}
\keyword{math}
\keyword{methods}

