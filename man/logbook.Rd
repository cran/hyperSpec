\name{logbook}
\alias{logbook}
\title{Logging the processing ot a hyperSpec Object
Extracts the slot \code{@log} of a \code{hyperSpec} object.}
\usage{
  logbook(x)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
}
\value{
  a \code{data.frame} containing \code{x@log}
}
\description{
  A \code{data.frame} in slot \code{x@log} keeps track of
  the changes done to the \code{hyperSpec} object.
}
\details{
  If option \code{log} is \code{TRUE}, entries will be
  generated automatically by hyperSpec functions that do
  assignments (see \code{\link{hy.setOptions}}). Entries
  can also be created manually via \code{\link{logentry}}.
}
\examples{
logbook (flu)
}
\author{
  C. Beleites
}
\seealso{
  \code{\link{hy.setOptions}}, \code{\link{logentry}}.
}

