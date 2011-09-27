\name{unittests}
\alias{hy.unittest}
\title{hyperSpec unit tests...}
\description{hyperSpec unit tests
If \code{\link[svUnit]{svUnit}} is available, run the unit tests and
display the results.}
\value{\code{NA} if \code{\link[svUnit]{svUnit}} is not available,
otherwise \code{TRUE} if all tests are passed successfully. If a test
fails, \code{hy.unittest} stops with an error.}
\author{C. Beleites}
\seealso{\code{\link[svUnit]{svUnit}}}
\keyword{programming}
\keyword{utilities}
\examples{
if (require (svUnit)){
hy.unittest ()
}
}

