\name{chondro}
\alias{chondro}
\docType{data}
\title{Raman spectra of 2 Chondrocytes in Cartilage}
\description{
A Raman-map (laterally resolved Raman spectra) of chondrocytes in cartilage. 
}
\usage{data(chondro)}
\format{
The data set has 796 Raman spectra measured on a 23 \eqn{\times}{x} 35 grid with 1
micron step size. Spatial information is in \code{chondro$x} and \code{chondro$y}.
Each spectrum has 300 data points in the range of ca. 600 - 1800 cm\eqn{^{-1}}{^-1}. 
}
\details{
See the vignette.
}
\author{A. Bonifacio and C. Beleites}
\examples{
data(chondro)

chondro

vignette ("chondro", package = "hyperSpec")
}
\keyword{datasets}
