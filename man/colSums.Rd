\name{colSums}
\alias{colMeans}
\alias{colMeans,hyperSpec-method}
\alias{colSums}
\alias{colSums,hyperSpec-method}
\alias{rowMeans}
\alias{rowMeans,hyperSpec-method}
\alias{rowSums}
\alias{rowSums,hyperSpec-method}
\title{functions for hyperSpec objects}
\usage{
  \S4method{colMeans}{hyperSpec}(x, na.rm = TRUE, ...,
    label.spc, user = NULL, short = "colMeans",
    date = NULL)

  \S4method{colSums}{hyperSpec}(x, na.rm = TRUE, ...,
    label.spc, user = NULL, short = "colSums", date = NULL)

  \S4method{rowMeans}{hyperSpec}(x, na.rm = TRUE, ...,
    label.wavelength, user = NULL, short = "rowMeans",
    date = NULL)

  \S4method{rowSums}{hyperSpec}(x, na.rm = TRUE, ...,
    label.wavelength, user = NULL, short = "rowSums",
    date = NULL)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{label.spc}{labels for the intensity axis for
  loadings-like (col) statistics}

  \item{label.wavelength}{labels for the wavelength axis
  for scores-like (row) statistics}

  \item{user,short,date}{handed to
  \code{\link[hyperSpec]{logentry}}}

  \item{na.rm,...}{further parameters to the base
  functions}
}
\description{
  hyperSpec objects can use the base functions
  \code{\link[base]{colMeans}},
  \code{\link[base]{colSums}}, \code{\link[base]{rowMeans}}
  and \code{\link[base]{rowSums}}.
}
\seealso{
  \link[base]{colSums}
}

