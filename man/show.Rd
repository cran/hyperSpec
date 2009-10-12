\name{show}
\docType{methods}
\alias{show,hyperSpec-method}
\alias{print,hyperSpec-method}
\alias{summary,hyperSpec-method}
\alias{as.character,hyperSpec-method}
\title{ Convert a hyerSpec object to character strings for Display}
\description{
  \code{print}, \code{show}, and \code{summary} show the result of \code{as.character}.
}
\usage{
\S4method{show}{hyperSpec}(object)
\S4method{print}{hyperSpec}(x, log = FALSE, \dots)
\S4method{summary}{hyperSpec}(object, log = TRUE, \dots)
\S4method{as.character}{hyperSpec}(x, digits = getOption ("digits"), max.print = 5, shorten.to = c(2, 1), log = TRUE)
}
\arguments{
  \item{x, object}{a \code{hyperSpec} object}
  
  \item{digits}{number of digits handed over to \code{format}}
  
  \item{max.print}{maximum number of elements to be printed (of a
  variable)}
  
  \item{shorten.to}{if a vector is longer than \code{max.print}, only
    the first \code{shorten.to[1]} and the last \code{shorten.to[2]}
    elements are printed}

  \item{log}{should the log be printed?}

  \item{\dots}{\code{print} hands any arguments to \code{as.character}}
}
\value{
  \code{as.character} returns a character vector fit to be printed by
  \code{cat} with \code{sep = "\\n"}.

  \code{print} invisibly returns \code{x} after printing, \code{show}
  returns an invisible \code{NULL}.
  }

\seealso{
  \code{\link[base]{print}},
  \code{\link[methods]{show}}, and
  \code{\link[base]{as.character}}
}
\examples{
chondro

print (chondro, log = TRUE)

logbook (chondro)
}  
\keyword{methods}
\keyword{print}
