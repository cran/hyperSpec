% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.txt.Horiba.R
\name{read.txt.Horiba}
\alias{read.txt.Horiba}
\alias{read.txt.Horiba.xy}
\alias{read.txt.Horiba.t}
\title{Import Horiba Labspec exported ASCII files}
\usage{
read.txt.Horiba(
  file,
  cols = c(spc = "I / a.u.", .wavelength = expression(Delta * tilde(nu)/cm^-1)),
  header = TRUE,
  sep = "\\t",
  row.names = NULL,
  check.names = FALSE,
  ...
)

read.txt.Horiba.xy(file, ...)

read.txt.Horiba.t(
  file,
  header = TRUE,
  sep = "\\t",
  row.names = NULL,
  check.names = FALSE,
  ...
)
}
\arguments{
\item{file}{connection (file name and path) to the .txt file}

\item{cols, header, sep, row.names, check.names, ...}{further parameters are handed over to \code{\link[hyperSpec]{read.txt.wide}}}
}
\value{
hyperSpec object
}
\description{
Read ASCII (.txt) files exported by Horiba's Labspec software (LabRAM spectrometers)
}
\details{
\code{read.txt.Horiba.xy} reads maps, i.e. .txt files where the first two columns give x and y coordinates.
}
\author{
C. Beleites
}
