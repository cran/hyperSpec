\name{normalize01}
\alias{normalize01}
\title{Normalize numbers -> [0, 1]}
\usage{
  normalize01(x, eps = .Machine$double.eps)
}
\arguments{
  \item{x}{vector with values to transform}

  \item{eps}{tolerance level for determining what is 0 and
  1}
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

