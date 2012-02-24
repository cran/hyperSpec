\name{qplotc}
\alias{qplotc}
\title{Spectra plotting with ggplot2}
\usage{
  qplotc(object, mapping = aes_string(x = "c", y = "spc"),
    ..., func = NULL, func.args = list())
}
\arguments{
  \item{object}{hyperSpec object}

  \item{mapping}{see \code{\link[ggplot2]{geom_point}}}

  \item{\dots}{handed to \code{\link[ggplot2]{geom_point}}}

  \item{func}{function to summarize the wavelengths, if
  \code{NULL}, only the first wavelength is used}

  \item{func.args}{arguments to \code{func}}
}
\value{
  a \code{\link[ggplot2]{ggplot}} object
}
\description{
  Spectra plotting with ggplot2
}
\details{
  These functions are still experimental and may change
  substantially in future.
}
\examples{
qplotc (flu)
qplotc (flu) + geom_smooth (method = "lm")
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link{plotc}}

  \code{\link[ggplot2]{ggplot}}\code{\link[ggplot2]{geom_point}}
}

