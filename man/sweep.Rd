\name{sweep}
\docType{methods}
\alias{sweep}
\alias{sweep-methods}
\alias{sweep,hyperSpec-method}
\title{Sweep Summary Statistic out of an hyperSpec Object}

\description{
  \code{\link[base]{sweep}} for \code{hyperSpec} objects.
}
\usage{
\S4method{sweep}{hyperSpec}(x, MARGIN, STATS, FUN = "-",
   check.margin = TRUE, \dots, 
   short = NULL, user = NULL, date = NULL)
}
\arguments{
  \item{x}{a \code{hyperSpec object.}}
  
  \item{MARGIN}{direction of the spectra matrix that \code{STATS} goees along.}

  \item{STATS}{the summary statistic to sweep out. Either a vector or a
	 \code{hyperSpec} object.}

  \item{FUN}{the function to do the sweeping, e.g. `-` or `/`.}

  \item{check.margin}{If \code{TRUE} (the default), warn if the length or
    dimensions of \code{STATS} do not match the specified dimensions
    of \code{x}.  Set to \code{FALSE} for a small speed gain when you
    \emph{know} that dimensions match.}

  \item{\dots}{further arguments for \code{FUN}}

  \item{short, user, date}{handed over to \code{\link{logentry}}.}
}
\details{
  Calls \code{\link[base]{sweep}} for the spectra matrix.

  \code{sweep} is useful for some spectra preprocessing, like
  offset correction, substraction of background spectra, and
  normalization of the spectra.
}
\value{
  A \code{hyperSpec} object.
}
\author{C. Beleites}
\seealso{\code{\link[base]{sweep}}}
\examples{
## Substract the background / slide / blank spectrum
# the example data does not have spectra of the empty slide,
# so instead the overall composition of the sample is substracted
background <- apply (chondro, 2, quantile, probs = 0.05)
corrected <- sweep (chondro, 2, background, "-")
plot (corrected, "spcprctl5")

## Offset correction
offsets <- apply (chondro, 1, min)
corrected <- sweep (chondro, 1, offsets, "-")
plot (corrected, "spcprctl5")

## Min-max normalization (on max amide I)
# the minimum is set to zero by the offset correction.
factor <- apply (corrected, 1, max)
corrected <- sweep (corrected, 1, factor, "/")
plot (corrected, "spcprctl5")
}

\keyword{methods}