\name{read.ini}
\alias{read.ini}
\title{Read INI files}
\usage{
  read.ini(con = stop("Connection con needed."))
}
\arguments{
  \item{con}{connection or file name}
}
\value{
  a list with one element per section in the .ini file,
  each containing a list with elements for the
  key-value-pairs.
}
\description{
  \code{read.ini} reads ini files of the form
}
\details{
  [section] key = value

  into a list.

  \code{read.ini} sanitizes the element names and tries to
  convert scalars and comma separated numeric vectors to
  numeric.
}
\author{
  C. Beleites
}
\keyword{file}
\keyword{IO}

