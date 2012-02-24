\name{read.txt.Shimadzu}
\alias{read.txt.Shimadzu}
\title{Reads Shimadzu Spectra Files (.txt) as exported by Shimadzu Chrome Solution (v. 2.73)}
\usage{
  read.txt.Shimadzu(filename, encoding = "", quiet = TRUE)
}
\arguments{
  \item{filename}{file name and path of the .txt file}

  \item{encoding}{encoding of the txt file (used by
  \code{\link[base]{readLines}})}

  \item{quiet}{suppress printing of progress}
}
\value{
  hyperSpec object
}
\description{
  Reads Shimadzu Spectra Files (.txt) as exported by
  Shimadzu Chrome Solution (v. 2.73)
}
\note{
  This is a first rough import function and the functions
  may change without notice.
}
\author{
  Bjoern Egert
}

