\name{scan.txt.Witec}
\alias{scan.txt.Witec}
\title{File Import Witec Raman}
\usage{
  scan.txt.Witec(filex = stop("filename needed"),
    filey = sub("-x", "-y", filex), points.per.line = NULL,
    lines.per.image = NULL, short = "scan.txt.Witec",
    user = NULL, date = NULL)
}
\arguments{
  \item{filex}{filename wavelength axis file}

  \item{filey}{filename intensity file}

  \item{points.per.line}{number of spectra in x direction
  of the map}

  \item{lines.per.image}{number of spectra in y direction}

  \item{short,user,date}{handed to \code{\link{logentry}}}
}
\value{
  a hyperSpec object
}
\description{
  Import Raman Spectra/Maps from Witec Instrument via ASCII
  files
}
\author{
  Claudia Beleites
}
\seealso{
  \code{vignette ("fileio")} for more information on file
  import.
}

