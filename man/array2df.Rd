\name{array2df}
\alias{array2df}
\title{array2df: Convert multidimensional array into
  matrix or data.frame}
\description{
  The "wide-format" array is converted into a "long-format" \code{matrix} or \code{data.frame}.
  }
\usage{
array2df(x, levels = rep(NA, length(dims)), matrix = FALSE, label.x = deparse(substitute(x)))
}

\arguments{
  \item{x}{\code{array}}
  \item{levels}{\code{list} with the levels for the dimensions of \code{x}.

    If \code{levels[[i]]} is \code{NULL} no column is produced for this
    factor. 
    
    If \code{levels[[i]]} is \code{NA}, the result column is a numeric
    with range from \code{1} to  \code{dim (x)[i]}

    \code{names(levels)} yield the resulting column names. 
  }
  \item{matrix}{If \code{TRUE}, a numeric \code{matrix} rather than a \code{data.frame}
    is returned.
  }
  \item{label.x}{Name for the column containing the \code{x} values.}
}
\details{
  If the resulting \code{data.frame} is too large to fit in memory, a
  \code{matrix} might help.

  The main benefit of this function is that it uses matrices as long as
  possible. This can give large advantages in terms of memory consumption.
}
\value{
  A \code{data.frame} or \code{matrix} with \code{prod (dim (x))} rows
  and \code{length (dim (x)) + 1} columns.
}
\author{C. Beleites}
\seealso{
  \code{\link[utils]{stack}}
}
\examples{
arr <- array (rnorm (24), 2:4)

array2df (arr)

array2df (arr, levels = list(NULL, x = NA, c = NULL), label.x = "value")

array2df (arr, levels = list(NULL, x = NA, c = letters [1:4]), label.x = "value")

summary (array2df (arr,
                   levels = list(NULL, x = NA, c = letters [1:4]),
                   label.x = "value"))

summary (array2df (arr,
                   levels = list(NULL, x = NA, c = letters [1:4]),
                   label.x = "value",
                   matrix = TRUE))

}
\keyword{array}
\keyword{manip}
