\name{spc.NA.linapprox}
\alias{spc.NA.linapprox}
\title{Impute missing data points}
\usage{
  spc.NA.linapprox(spc, neighbours = 1, ...,
    short = "spc.NA.linapprox", user = NULL, date = NULL)
}
\arguments{
  \item{spc}{hyperSpec object with spectra matrix
  containing \code{NA}s}

  \item{neighbours}{how many neighbour data points should
  be used to fit the line}

  \item{\dots}{ignored}

  \item{short,user,date}{handed to \code{\link{logentry}}}
}
\value{
  hyperSpec object
}
\description{
  Replace \code{NA}s in the spectra matrix by linear
  interpolation.
}
\author{
  Claudia Beleites
}

