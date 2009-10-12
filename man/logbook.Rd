\name{logbook}
\Rdversion{1.1}
\alias{logbook}
\title{Logging the processing ot a hyperSpec Object}
\description{
Extracts the slot \code{@log} of a \code{hyperSpec} object.
}
\usage{
logbook(x)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
}
\details{
A \code{data.frame} in slot \code{x@log} keeps track of the changes done to the \code{hyperSpec} object.
}
\value{
a \code{data.frame} containing \code{x@log}}
\author{C. Beleites}
\examples{
logbook (flu)
}
