\name{collapse}
\alias{collapse}
\alias{collapse.hyperSpec}
\title{Collapse hyperSpec objects}
\usage{
  collapse(..., short.log = TRUE, short = "collapse",
    user = NULL, date = NULL)
}
\arguments{
  \item{\dots}{hyperSpec objects to be collapsed into one
  object. Instead of giving several arguments, a list with
  all objects to be collapsed may be given.}

  \item{short.log}{if \code{TRUE}, only the dimensions of
  the hyperSpec objects are logged by \code{collapse}.}

  \item{short,user,date}{handed over to \code{logentry}}
}
\value{
  a hyperSpec object
}
\description{
  collapse/bind several hyperSpec objects into one object
}
\details{
  The spectra from all objects will be put into one object.
  The resulting object has all wavelengths that occur in
  the input objects. Data points corresponding to
  wavelengths not in the original spectrum will be set to
  NA. Extra data is combined in the same manner.
}
\examples{
barbiturates [1:3]
barb <- collapse (barbiturates [1:3])
barb

a <- barbiturates [[1]]
b <- barbiturates [[2]]
c <- barbiturates [[3]]

a
b
c
collapse (a, b, c)
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[base]{merge}} to merge hyperSpec objects that
  share wavelengths but contain different spectra,
  \code{\link[base]{rbind}}, and
  \code{\link[plyr]{rbind.fill}} for
}
\keyword{manip}

