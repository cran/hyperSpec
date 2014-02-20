\name{scan.dat.Witec}
\alias{scan.dat.Witec}
\alias{scan.txt.Witec}
\title{File Import Witec Raman}
\usage{
  scan.dat.Witec(filex = stop("filename or connection needed"),
    filey = sub("-x", "-y", filex), points.per.line = NULL,
    lines.per.image = NULL, ...)

  scan.txt.Witec(file = stop("filename or connection needed"),
    points.per.line = NULL, lines.per.image = NULL, ...,
    nwl = 1024)
}
\arguments{
  \item{filex}{filename wavelength axis file}

  \item{filey}{filename intensity file}

  \item{points.per.line}{number of spectra in x direction
  of the map}

  \item{lines.per.image}{number of spectra in y direction}

  \item{...}{handed to \code{\link[base]{scan}}}

  \item{nwl}{number of wavelengths, if \code{NULL},
  \code{readLines} is used to determine \code{nwl}
  automatically.}

  \item{file}{filename or connection to ASCII file}
}
\value{
  a hyperSpec object
}
\description{
  Import Raman Spectra/Maps from Witec Instrument via ASCII
  files
}
\details{
  \code{scan.dat.Witec} reads Witec's ASCII exported data
  which comes in separate files with x and y data.
  \code{scan.txt.Witec} reads Witec ASCII files where the
  first column gives the wavelength axes and the other
  columns the spectra.
}
\author{
  Claudia Beleites
}
\seealso{
  \code{vignette ("fileio")} for more information on file
  import.
}

