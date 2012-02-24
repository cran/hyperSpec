\docType{methods}
\name{sample}
\alias{isample}
\alias{sample}
\alias{sample,data.frame-method}
\alias{sample,hyperSpec-method}
\alias{sample,matrix-method}
\title{Random Samples and Permutations
Take a sample of the specified size from the elements of x with or without
replacement.}
\usage{
  \S4method{sample}{hyperSpec}(x, size = nrow(x),
    replace = FALSE, prob = NULL, short = "sample",
    user = NULL, date = NULL)

  isample(x, size = nrow(x), replace = FALSE, prob = NULL)

  \S4method{sample}{data.frame}(x, size = nrow(x),
    replace = FALSE, prob = NULL, drop = FALSE)

  \S4method{sample}{matrix}(x, size = nrow(x),
    replace = FALSE, prob = NULL, drop = FALSE)
}
\arguments{
  \item{x}{The hyperSpec object, data.frame or matrix to
  sample fromto sample from}

  \item{size}{positive integer giving the number of spectra
  (rows) to choose.}

  \item{replace}{Should sampling be with replacement?}

  \item{prob}{A vector of probability weights for obtaining
  the elements of the vector being sampled.}

  \item{short}{date are handed to \code{\link{logentry}}}

  \item{user}{date are handed to \code{\link{logentry}}}

  \item{date}{are handed to \code{\link{logentry}}}

  \item{drop}{see \code{\link[base]{drop}}: by default, do
  not drop dimensions of the result}
}
\value{
  a hyperSpec object, data.frame or matrix with \code{size}
  rows for \code{sample}, and an integer vector for
  \code{isample} that is suitable for indexing (into the
  spectra) of x.

  vector with indices suitable for row-indexing x
}
\description{
  Random Samples and Permutations Take a sample of the
  specified size from the elements of x with or without
  replacement.

  \code{isample} returns an vector of indices,
  \code{sample} returns the corresponding hyperSpec object.
}
\examples{
sample (flu, 3)

plot (flu, col = "darkgray")
plot (sample (flu, 3), col = "red", add = TRUE)

plot (flu, col = "darkgray")
plot (sample (flu, 3, replace = TRUE), col = "#0000FF80", add = TRUE,
      lines.args = list (lwd = 2));
isample (flu, 3)
isample (flu, 3, replace = TRUE)
isample (flu, 8, replace = TRUE)
sample (cars, 2)
sample (matrix (1:24, 6), 2)
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[base]{sample}}
}
\keyword{distribution}
\keyword{methods}

