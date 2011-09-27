\name{rbind-fill}
\alias{rbind.fill.matrix}
\title{Bind matrices by row, and fill missing columns with NA...}
\usage{rbind.fill.matrix(...)

rbind.fill.matrix(...)


rbind.fill(...)

}
\description{Bind matrices by row, and fill missing columns with NA}
\details{\code{rbind.fill.matrix}: The matrices are bound together using their column names or the column indices (in that order of
precedence.) Numeric columns may be converted to character beforehand, e.g. using format.  If a
matrix doesn't have colnames, the column number is used (via \code{\link[base]{make.names}(unique
= TRUE)}).

Note that this means that a column with name \code{"X1"} is merged with the first column of a
matrix without name and so on.

Vectors are converted to 1-column matrices prior to rbind.

Matrices of factors are not supported. (They are anyways quite inconvenient.) You may convert
them first to either numeric or character matrices. If a character matrix is merged with a
numeric, the result will be character.

Row names are ignored.

The return matrix will always have column names.

\code{rbind.fill.matrix}: The matrices are bound together using their column names or the column indices (in that order of
precedence.) Numeric columns may be converted to character beforehand, e.g. using format.  If a
matrix doesn't have colnames, the column number is used (via \code{\link[base]{make.names}(unique
= TRUE)}).

Note that this means that a column with name \code{"X1"} is merged with the first column of a
matrix without name and so on.

Vectors are converted to 1-column matrices prior to rbind.

Matrices of factors are not supported. (They are anyways quite inconvenient.) You may convert
them first to either numeric or character matrices. If a character matrix is merged with a
numeric, the result will be character.

Row names are ignored.

The return matrix will always have column names.


\code{rbind.fill}: This is an enhancement to \code{\link{rbind}} which adds in columns
that are not present in all inputs, accepts a list of data frames, and 
operates substantially faster


}
\alias{rbind.fill}
\author{C. Beleites}
\seealso{\code{\link[base]{rbind}}, \code{\link[base]{cbind}}, \code{\link[plyr]{rbind.fill}}\code{\link[base]{rbind}}, \code{\link[base]{cbind}}, \code{\link[plyr]{rbind.fill}}}
\keyword{manip}
\value{\code{rbind.fill.matrix}: a matrix

\code{rbind.fill.matrix}: a matrix


}
\examples{
A <- matrix (1:4, 2)
B <- matrix (6:11, 2)
A
B
rbind.fill.matrix (A, B)

colnames (A) <- c (3, 1)
A
rbind.fill.matrix (A, B)

rbind.fill.matrix (A, 99)

A <- matrix (1:4, 2)
B <- matrix (6:11, 2)
A
B
rbind.fill.matrix (A, B)

colnames (A) <- c (3, 1)
A
rbind.fill.matrix (A, B)

rbind.fill.matrix (A, 99)

#' rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])
}
\alias{rbind.fill.matrix}
\alias{rbind.fill}
\keyword{manip}
\alias{rbind.fill}
\keyword{manip}
\arguments{\item{\dots}{data frames to row bind together}
}

