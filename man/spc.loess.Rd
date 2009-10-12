\name{spc.loess}
\alias{spc.loess}
\title{ loess smoothing interpolation for spectra}
\description{
  Spectra can be smoothed and interpolated on a new wavelength axis using \code{\link[stats]{loess}}. 
  }
\usage{
spc.loess(spc, newx, \dots, short = NULL, user = NULL, date = NULL)
}
\arguments{
  \item{spc}{the \code{hyperSpec} object}
  \item{newx}{wavelengh axis tointrpolate on}
  \item{\dots}{parameters for \code{\link[stats]{loess}} and \code{\link[stats]{loess.control}}.
 
If \code{enp.target} is not given, it defaults to \code{nwl (spc) / 4}.

If \code{surface} is not given, it defaults to \code{"direct"}.
  
  }
  \item{short, user, date}{handed to \code{\link{logentry}}.}
}
\details{
	Applying \code{\link[stats]{loess}} to each of the spectra, 
	an interpolation onto a new wavelength axis is performed. 
	At the same time, the specta are smoothed in order to increase
	the signal : noise ratio. See  \code{\link[stats]{loess}} and 
	\code{\link[stats]{loess.control}} on the parameters that control 
	the amount of smoothing.  
}
\value{
  a new \code{hyperspec} object.
}
\author{C. Beleites}
\seealso{\code{\link[stats]{loess}}, \code{\link[stats]{loess.control}}}
\examples{
plot (flu)
plot (spc.loess(flu, seq (420, 470, 5), enp.target = 15, surface = "direct"), 
      add = TRUE, col = "red", lines.args = list (type = "b", pch = 20))
}
\keyword{manip}
\keyword{datagen}
