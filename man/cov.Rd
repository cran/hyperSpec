\name{cov}
\alias{cov}
\alias{cov,hyperSpec,missing-method}
\alias{pooled.cov}
\title{Covariance matrices for hyperSpec objects}
\usage{
  \S4method{cov}{hyperSpec,missing}(x, y, use, method)

  pooled.cov(x, groups, ...,
    regularize = 1e-05 * max(abs(COV)))
}
\arguments{
  \item{x}{hyperSpec object}

  \item{y}{not supported}

  \item{use,method}{handed to \code{\link[stats]{cov}}}

  \item{groups}{factor giving the groups

  factor indicating the groups}

  \item{...}{ignored}

  \item{regularize}{regularization of the covariance
  matrix. Set \code{0} to switch off

  \code{pooled.cov} calculates pooled covariance like e.g.
  in LDA.}
}
\value{
  covariance matrix of size \code{nwl (x)} x \code{nwl (x)}
}
\description{
  Covariance matrices for hyperSpec objects
}
\examples{
image (cov (chondro))
pcov <-  pooled.cov (chondro, chondro$clusters)
plot (pcov$means)
image (pcov$COV)
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[stats]{cov}}
}

