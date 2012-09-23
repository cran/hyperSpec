\name{normalize01}
\alias{normalize01}
\alias{normalize01,hyperSpec-method}
\alias{normalize01,matrix-method}
\alias{normalize01,numeric-method}
\title{Normalize numbers -> [0, 1]}
\usage{
  \S4method{normalize01}{matrix}(x,
    eps = .Machine$double.eps)

  \S4method{normalize01}{numeric}(x,
    eps = .Machine$double.eps)

  \S4method{normalize01}{hyperSpec}(x, ...)
}
\arguments{
  \item{x}{vector with values to transform}

  \item{eps}{tolerance level for determining what is 0 and
  1}

  \item{\dots}{additional parameters such as \code{eps}
  handed down.}
}
\value{
  vector with \code{x} values mapped to the interval [0, 1]
}
\description{
  The input \code{x} is mapped to [0, 1] by subtracting the
  minimum and subsequently dividing by the maximum. If all
  elements of \code{x} are equal, 1 is returned.
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[hyperSpec]{wl.eval}},
  \code{\link[hyperSpec]{vanderMonde}}
}

