\name{spc.identify}
\alias{spc.identify}
\title{Identifying Spectra and Spectral Data Points}
\description{
  This function allows to identify the spectrum and the wavelength of a
  point in a plot produced by \code{\link{plotspc}}.
}
\usage{
spc.identify(x, y = NULL, wavelengths = NULL, ispc = NULL, \dots)
}
\arguments{
  \item{x}{either the abscissa coordinates or the list returned by
	 \code{\link{plotspc}}} 
  \item{y}{the ordinate values. Giving \code{y} will override any values
	 from \code{x$y}.}
  \item{wavelengths}{the wavelengths for the data points. Giving
	 \code{wavelengths} will override any values from
	 \code{x$wavelengths}.}
  \item{ispc}{if a selection of spectra was plotted, their indices can
	 be given in \code{ispc}. In this case \code{ispc [i]} is returned
	 rather than \code{i}.}
  \item{\dots}{passed to \code{\link[graphics]{identify}}}
}
\details{
  The function uses \code{\link[graphics]{identify}} to identify the
  points clicked at with the left button and converts them into a more
  convenient format.
}
\value{
  a \code{data.frame} with columns
  \item{i}{spectra indices of the identified points, i.e. the rows of
	 the \code{hyperSpec} object that was plotted.
 
	 If \code{ispc} is given, \code{ispc [i]} is returned rather than
  \code{i}. 
  }
  \item{wavelengths}{the wavelengths of the identified points}
}
\author{C. Beleites}
\seealso{\code{\link[graphics]{identify}}, \code{\link{plotspc}}}
\examples{
\dontrun{
ispc <- sample (nrow (laser), 10)
ispc

identified <- spc.identify (plotspc (laser[ispc]))
## convert to the "real" spectra indices
ispc [identified$i]
identified$wavelength

spc.identify (plotspc (laser[ispc]), ispc = ispc)
}
}
\keyword{iplot}

