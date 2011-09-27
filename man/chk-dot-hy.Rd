\name{chk-dot-hy}
\alias{chk.hy}
\title{Validation of hyperSpec objects}
\usage{chk.hy(object)
}
\description{Check whether an object is a hyperSpec object and validate the object}
\alias{validObject}
\alias{validObject,hyperSpec-method}
\alias{chk.hy}
\author{C. Beleites}
\seealso{\code{\link[methods]{validObject}}}
\value{\code{TRUE} if the check passes, otherwise stop with an
error.}
\keyword{methods}
\arguments{\item{object}{the object to check}
}
\examples{
chk.hy (chondro)
validObject (chondro)
}

