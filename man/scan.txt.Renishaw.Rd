\name{scan.txt.Renishaw}
\alias{scan.txt.Renishaw}
\title{import Raman measurements from Renishaw ASCII-files}
\usage{scan.txt.Renishaw(file=stop("filename is required"), data="xyspc",
    nlines=0, nspc, \dots)}
\description{import Raman measurements from Renishaw .txt file}
\details{Renishaw .wxd files are converted to .txt ASCII files by their batch converter.
They come in a "long" format with columns (y x | time | z)? wavelength intensity. 
The first columns depend on the data type.

The corresponding possibilities for the \code{data} argument are:
\tabular{lll}{
\code{data}                   \tab columns    \tab \cr
\code{"spc"}                  \tab wl int     \tab single spectrum \cr
\code{"zspc"}, \code{"depth"} \tab z wl int   \tab depth profile\cr
\code{"ts"}                   \tab t wl int   \tab time series\cr
\code{"xyspc"}                \tab y x wl int \tab 2d map\cr
       }
       
This function allows reading very large ASCII files, but it does not work on files with missing 
values (\code{NA}s are allowed).  

If the file is so large that it sould be read in chunks and \code{nspc} is not given, 
\code{scan.txt.Renishaw} tries to guess it by using \code{wc} (if installed).
}
\value{the \code{hyperSpec} object}
\seealso{\code{\link{read.txt.long}}, \code{\link{read.txt.wide}}, \code{\link[base]{scan}}}
\author{C. Beleites}
\arguments{\item{file}{file name}
\item{data}{type of file, one of "spc", "xyspc", "zspc", "depth", "ts", see details.}
\item{nlines}{number of lines to read in each chunk, if 0 or less read whole file at once}
\item{nspc}{number of spectra in the file}
\item{\dots}{passed to logentry}}
\keyword{IO}
\keyword{file}
