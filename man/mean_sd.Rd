\name{mean_sd}
\alias{mean_sd}
\alias{mean_pm_sd}
\title{Mean and Standard Deviation}
\description{
  Calculate mean and standard deviation, and mean, mean \eqn{\pm}{+-} one
  standard deviation, respectively.
}
\usage{
mean_sd(x, na.rm = TRUE)
mean_pm_sd(x, na.rm = TRUE)
}
\arguments{
  \item{x}{ a numeric vector }
  \item{na.rm}{handed to \code{\link[base]{mean}} and \code{\link[stats]{sd}}}
}
\details{
  These functions are provided for convenience. 
}
\value{
  \code{mean_sd} returns a vector with two values (mean and standard
  deviation) of \code{x}.

  \code{mean_pm_sd} returns a vector with 3 values: mean - 1 sd, mean,
  mean + 1 sd.
}
\author{C. Beleites}
\seealso{\code{\link[base]{mean}}, \code{\link[stats]{sd}}}
\examples{
  mean_sd (flu$c)
  mean_pm_sd (flu$c)
}
\keyword{univar}

