\name{spc-bin}
\alias{spc.bin}
\title{Wavelength Binning...}
\usage{spc.bin(spc, by=stop("reduction factor needed"), na.rm=TRUE, ...,
    short="spc.bin", user=NULL, date=NULL)
}
\description{Wavelength Binning
In order to reduce the spectral resolution and thus gain signal to noise
ratio or to reduce the dimensionality of the spectral data set, the
spectral resolution can be reduced.}
\details{The mean of every \code{by} data points in the spectra is calculated.

Using \code{na.rm = TRUE} always takes about twice as long as \code{na.rm = FALSE}.

If the spectra matrix does not contain too many \code{NA}s, \code{na.rm = 2} is faster than
\code{na.rm = TRUE}.}
\value{A \code{hyperSpec} object with \code{ceiling (nwl (spc) / by)} data points per spectrum.}
\author{C. Beleites}
\keyword{manip}
\keyword{datagen}
\arguments{\item{spc}{the \code{hyperSpec} object}
\item{by}{reduction factor}
\item{na.rm}{decides about the treatment of \code{NA}s:

if \code{FALSE} or \code{0}, the binning is done using \code{na.rm = FALSE}

if \code{TRUE} or \code{1}, the binning is done using \code{na.rm = TRUE}

if \code{2}, the binning is done using \code{na.rm = FALSE}, and resulting \code{NA}s are
corrected with \code{mean(\dots{}, na.rm = TRUE)}.}
\item{...}{ignored}
\item{short,user,date}{handed to \code{\link[hyperSpec]{logentry}}.}
}
\examples{
spc <- spc.bin (flu, 5)

plot (flu[1,,425:475])
plot (spc[1,,425:475], add = TRUE, col = "blue")

nwl (flu)
nwl (spc)
}

