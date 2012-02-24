\name{read.jdx.Shimadzu}
\alias{read.jdx.Shimadzu}
\title{JCAMP-DX Import for Shimadzu Library Spectra}
\usage{
  read.jdx.Shimadzu(filename, encoding = "")
}
\arguments{
  \item{filename}{file name and path of the .jdx file}

  \item{encoding}{encoding of the JCAMP-DX file (used by
  \code{\link[base]{readLines}})}
}
\value{
  hyperSpec object
}
\description{
  this is a first rough import function for JCAMP-DX
  spectra as exported by the Shimadzu GCxGC-MS library
}
\note{
  JCAMP-DX support is very incomplete and the functions may
  change without notice.
}
\author{
  C. Beleites
}

