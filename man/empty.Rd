\name{empty}
\alias{empty}
\title{Empty hyperSpec object}
\usage{
  empty(x, nrow = 0, spc = NA, extra = NA, short = "empty",
    user = NULL, date = NULL)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{nrow}{number of rows the new object should have}

  \item{spc}{value to initialize the new spectra matrix
  with}

  \item{extra}{value to initialize the new extra data with}

  \item{short}{handed to logentry}

  \item{user}{handed to logentry}

  \item{date}{handed to logentry}
}
\description{
  Empty produces an hyperSpec object with the same columns
  and wavelengths as \code{x}.  The new object will either
  contain no rows at all (default), or the given number of
  rows with all data initialized to \code{spc} and
  \code{extra}, respectively.
}
\examples{
empty (chondro, nrow = 2, spc = 0)
}
\author{
  C. Beleites
}
\keyword{manip}

