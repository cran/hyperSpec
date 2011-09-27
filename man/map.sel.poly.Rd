\name{map.sel.poly}
\alias{sel.poly}
\title{Interactively select a polygon (grid graphics)...}
\usage{sel.poly(pch=19, size=0.3, ...)
}
\description{Interactively select a polygon (grid graphics)}
\details{Click the points that should be connected as polygon. Input ends with right click (see
\code{\link[grid]{grid.locator}}).}
\value{n x 2 matrix with the points of the open polygon}
\author{Claudia Beleites}
\seealso{\code{\link[grid]{grid.locator}}}
\keyword{iplot}
\arguments{\item{pch}{symbol to display the points of the polygon}
\item{size}{size for polygon point symbol}
\item{...}{further arguments for \code{\link[grid]{grid.points}} and
\code{\link[grid]{grid.lines}}}
}

