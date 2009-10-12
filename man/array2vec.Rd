\name{array2vec}
\alias{array2vec}
\alias{vec2array}
\title{Converting array and vector Indices}
\description{
  Calculate the vector index from array indices, and vice versa.
}
\usage{
array2vec(iarr, dim)
vec2array(ivec, dim)
}

\arguments{
  \item{iarr}{vector with the indices into the array dimensions}
  \item{ivec}{scalar with the index into the vector}
  \item{dim}{vector with the array dimensions, as returned by \code{dim (x)}}
}
\details{
  \code{array}s are \code{numeric}s with a \code{dim} attribute and are
  stored with the first index moving fastest (i.e. by column). They can
  be indexed both ways. 
}
\value{
  \code{array2vec} returns a scalar, \code{vec2array} a \code{matrix}. 
}
\author{C. Beleites}
\seealso{see \code{\link[base]{Extract}} on the difference of indexing
  an \code{array} with a vector or a \code{matrix}.}
\examples{
arr <- array (rnorm (24), dim = 2 : 4)
arr

v <- matrix(c(2, 2, 2), nrow = 1)
i <- array2vec (v, dim = dim (arr))
i
arr[v]
arr[i]

arr[c(2, 2, 2)] ## indexing with a vector
arr[2]

i <- 14
v <- vec2array (i, dim = dim (arr))
v
arr [v]
arr [i]
}
\keyword{array}

