\name{palettes}
\alias{matlab.palette}
\title{Matlab-like Palettes...}
\usage{matlab.palette(n=100, ...)

matlab.dark.palette(n=100, ...)


alois.palette(n=100, ...)

}
\description{Matlab-like Palettes
Two palettes going from blue over green to red, approximately as the
standard palette of Matlab does. The second one has darker green values and
is better suited for plotting lines on white background.}
\alias{matlab.palette}
\value{\code{matlab.palette}: A vector containing the color values in the form "\#rrbbggaa".

}
\author{C. Beleites and A. Bonifacio}
\seealso{\code{\link[grDevices]{rainbow}}}
\keyword{color}
\arguments{\item{n}{the number of colors to be in the palette.}
\item{\dots}{further arguments are handed to \code{\link[grDevices]{rainbow}}
(\code{alois.palette}: \code{\link[grDevices]{colorRampPalette}})}
}
\examples{
plotmap (chondro [,, 778], col.regions = matlab.palette ())

plot (flu, col = matlab.dark.palette (nrow (flu)))

plotmap (chondro, col = alois.palette)
}
\alias{matlab.dark.palette}
\alias{matlab.dark.palette}
\alias{alois.palette}

