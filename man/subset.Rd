\docType{methods}
\name{subset}
\alias{subset}
\alias{subset,hyperSpec-method}
\title{subset}
\usage{
  \S4method{subset}{hyperSpec}(x, ..., short = "subset",
    date = NULL, user = NULL)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{\dots}{handed to \code{\link[base]{subset}}
  (data.frame method)}

  \item{short,user,date}{passed to
  \code{\link[hyperSpec]{logentry}}}
}
\value{
  hyperSpec object containing the respective subset of
  spectra.
}
\description{
  subset for hyperSpec object
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[base]{subset}}
}

