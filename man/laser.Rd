\name{laser}
\alias{laser}
\docType{data}
\title{Laser Emission}
\description{
A time series of an unstable laser emission.
}
\usage{data(laser)}
\format{
 The data set consists of  84 laser emission spectra measured during 95 min.
 Each spectrum has 36 data points in the range of 404.5 nm to 405.8 nm. 
}
\details{
  see the Vignette
}
\author{C. Beleites}
\examples{
laser

vignette ("laser", package="hyperSpec")

plot (laser[,,405], "ts", plot.args = list (type = "b"))
}
\keyword{datasets}
