\name{spc.loess}
\alias{spc.loess}
\title{loess smoothing interpolation for spectra
Spectra can be smoothed and interpolated on a new wavelength axis using
\code{\link[stats]{loess}}.}
\usage{
  spc.loess(spc, newx, enp.target = nwl(spc)/4,
    surface = "direct", ..., short = "spc.loess",
    user = NULL, date = NULL)
}
\arguments{
  \item{spc}{the \code{hyperSpec} object}

  \item{newx}{wavelengh axis tointrpolate on}

  \item{enp.target,surface,\dots}{parameters for
  \code{\link[stats]{loess}} and
  \code{\link[stats]{loess.control}}.}

  \item{short,user,date}{handed to \code{\link{logentry}}.}
}
\value{
  a new \code{hyperspec} object.
}
\description{
  Applying \code{\link[stats]{loess}} to each of the
  spectra, an interpolation onto a new wavelength axis is
  performed.  At the same time, the specta are smoothed in
  order to increase the signal : noise ratio. See
  \code{\link[stats]{loess}} and
  \code{\link[stats]{loess.control}} on the parameters that
  control the amount of smoothing.
}
\examples{
plot (flu, col = "darkgray")
plot (spc.loess(flu, seq (420, 470, 5)), add = TRUE, col = "red")

flu [[3, ]] <- NA_real_
smooth <- spc.loess(flu, seq (420, 470, 5))
smooth [[, ]]
plot (smooth, add = TRUE, col = "blue")
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[stats]{loess}},
  \code{\link[stats]{loess.control}}
}
\keyword{datagen}
\keyword{manip}

