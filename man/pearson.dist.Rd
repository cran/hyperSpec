\name{pearson.dist}
\alias{pearson.dist}
\title{Calculate Distance based on Pearson's R\^2}
\description{
The calculated distance is
\eqn{D^2 = \frac{1 - COR (\code{x}')}{2}}{D^2 = (1 - COR (x')) / 2}      
}
\usage{
pearson.dist(x)
}
\arguments{
  \item{x}{a matrix}
}
\details{
 The distance between the rows of \code{x} is calculated. 
 The possible values range from 0 (prefectly correlated) over 0.5 (uncorrelated) to 1 (perfectly anti-correlated).
}
\value{
 distance matrix (distance object) 
}
\references{S. Theodoridis and K. Koutroumbas: Pattern Recognition, 3rd ed., p0 }
\author{C. Beleites}
\seealso{ \code{\link[stats]{as.dist}}}
\examples{
dist <- pearson.dist (flu[[]])
}
\keyword{cluster}
