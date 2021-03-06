% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R, R/read.txt.Renishaw.R
\name{scan.txt.Renishaw}
\alias{scan.txt.Renishaw}
\alias{scan.zip.Renishaw}
\alias{read.txt.Renishaw}
\alias{read.zip.Renishaw}
\title{import Raman measurements from Renishaw ASCII-files}
\usage{
scan.txt.Renishaw(...)

scan.zip.Renishaw(...)

read.txt.Renishaw(
  file = stop("file is required"),
  data = "xyspc",
  nlines = 0,
  nspc = NULL
)

read.zip.Renishaw(
  file = stop("filename is required"),
  txt.file = sub("[.]zip", ".txt", basename(file)),
  ...
)
}
\arguments{
\item{...}{Arguments for \code{read.txt.Renishaw}}

\item{file}{file name or connection}

\item{data}{type of file, one of "spc", "xyspc", "zspc", "depth", "ts", see
details.}

\item{nlines}{number of lines to read in each chunk, if 0 or less read
  whole file at once.

\code{nlines} must cover at least one complete spectrum,i.e. \code{nlines}
  must be at least the number of data points per spectrum. Reasonable
  values start at \code{1e6}.}

\item{nspc}{number of spectra in the file}

\item{txt.file}{name of the .txt file in the .zip archive. Defaults to zip
file's name with suffix .txt instead of .zip}
}
\value{
the \code{hyperSpec} object
}
\description{
import Raman measurements from Renishaw (possibly compressed) .txt file.
}
\details{
The file may be of any file type that can be read by
\code{\link[base]{gzfile}} (i.e. text, or zipped by gzip, bzip2, xz or
lzma). .zip zipped files need to be read using \code{read.zip.Renishaw}.

Renishaw .wxd files are converted to .txt ASCII files by their batch
converter. They come in a "long" format with columns (y x | time | z)?
wavelength intensity.  The first columns depend on the data type.

The corresponding possibilities for the \code{data} argument are:
\tabular{lll}{ \code{data} \tab columns \tab \cr \code{"spc"} \tab wl int
\tab single spectrum \cr \code{"zspc"}, \code{"depth"} \tab z wl int \tab
depth profile\cr \code{"ts"} \tab t wl int \tab time series\cr
\code{"xyspc"} \tab y x wl int \tab 2d map\cr }

This function allows reading very large ASCII files, but it does not work
on files with missing values (\code{NA}s are allowed).

If the file is so large that it sould be read in chunks and \code{nspc} is
not given, \code{read.txt.Renishaw} tries to guess it by using \code{wc}
(if installed).
}
\seealso{
\code{\link{read.txt.long}}, \code{\link{read.txt.wide}},
  \code{\link[base]{scan}}
}
\author{
C. Beleites
}
\keyword{IO}
\keyword{file}
\keyword{internal}
