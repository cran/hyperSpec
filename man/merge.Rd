\docType{methods}
\name{merge}
\alias{merge}
\alias{merge,hyperSpec,hyperSpec-method}
\title{Merge hyperSpec objects}
\usage{
  \S4method{merge}{hyperSpec,hyperSpec}(x, y, ...,
    short = "merge", user = NULL, date = NULL)
}
\arguments{
  \item{x}{a hyperSpec object}

  \item{y}{a hyperSpec object}

  \item{\dots}{handed to
  \code{\link[base]{merge.data.frame}}}

  \item{short,user,date}{handed to \code{\link{logentry}}}
}
\description{
  Merges two hyperSpec objects and
  \code{\link[base]{cbind}}s their spectra matrices.
}
\details{
  After merging, the spectra matrix can contain duplicates,
  and is not ordered according to the wavelength.

  If the wavelength axis should be ordered, use
  \code{\link{orderwl}}.
}
\examples{
merge (chondro [1:10,, 600], chondro [5:15,, 600], by = c("x", "y"))$.
tmp <- merge (chondro [1:10,, 610], chondro [5:15,, 610],
              by = c("x", "y"), all = TRUE)
tmp$.
wl (tmp)
\dontshow{
 stopifnot (nrow (merge (chondro [1:10], chondro [5:15], all = TRUE)) == 15)
 stopifnot (nrow (merge (chondro [1:10], chondro [5:15])) == 6)
}

## remove duplicated wavelengths:
approxfun <- function (y, wl, new.wl){
  approx (wl, y, new.wl, method = "constant",
          ties = function (x) mean (x, na.rm = TRUE)
          )$y
}

merged <- merge (chondro [1:7,, 610 ~ 620], chondro [5:10,, 615 ~ 625], all = TRUE)
merged$.
merged <- apply (merged, 1, approxfun,
                 wl = wl (merged), new.wl = unique (wl (merged)),
                 new.wavelength = "new.wl")
merged$.
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[base]{merge}}.

  \code{\link{collapse}} combines hyperSpec objects that do
  not share the wavelength axis. \code{\link{rbind}}, and
  \code{\link{cbind}} for combining hyperSpec objects that.
}
\keyword{manip}

