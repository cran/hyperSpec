\name{matlab.palette}
\alias{alois.palette}
\alias{matlab.dark.palette}
\alias{matlab.palette}
\title{Matlab-like Palettes
Two palettes going from blue over green to red, approximately as the
standard palette of Matlab does. The second one has darker green values and
is better suited for plotting lines on white background.}
\usage{
  matlab.palette(n = 100, ...)

  matlab.dark.palette(n = 100, ...)

  alois.palette(n = 100, ...)
}
\arguments{
  \item{n}{the number of colors to be in the palette.}

  \item{\dots}{further arguments are handed to
  \code{\link[grDevices]{rainbow}} (\code{alois.palette}:
  \code{\link[grDevices]{colorRampPalette}})}
}
\value{
  A vector containing the color values in the form
  "\#rrbbggaa".
}
\description{
  Matlab-like Palettes Two palettes going from blue over
  green to red, approximately as the standard palette of
  Matlab does. The second one has darker green values and
  is better suited for plotting lines on white background.
}
\examples{
plotmap (chondro [,, 778], col.regions = matlab.palette ())
plot (flu, col = matlab.dark.palette (nrow (flu)))
plotmap (chondro, col = alois.palette)
}
\author{
  C. Beleites and A. Bonifacio
}
\seealso{
  \code{\link[grDevices]{rainbow}}
}
\keyword{color}

