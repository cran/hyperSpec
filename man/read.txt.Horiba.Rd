\name{read.txt.Horiba}
\alias{read.txt.Horiba}
\alias{read.txt.Horiba.t}
\alias{read.txt.Horiba.xy}
\title{Import Horiba Labspec exported ASCII files}
\usage{
  read.txt.Horiba(file,
    cols = c(spc = "I / a.u.", .wavelength = expression(Delta * tilde(nu)/cm^-1)),
    header = TRUE, sep = "\t", row.names = NULL,
    check.names = FALSE, ..., remove.zerospc = TRUE)

  read.txt.Horiba.xy(file, ...)

  read.txt.Horiba.t(file, header = TRUE, sep = "\t",
    row.names = NULL, check.names = FALSE, ...,
    remove.zerospc = TRUE)
}
\arguments{
  \item{file}{connection (file name and path) to the .txt
  file}

  \item{remove.zerospc}{LabRAM saves spectra consisting of
  zeros only if e.g. a map was aborted. The default is to
  remove these spectra.}

  \item{cols,header,sep,row.names,check.names,...}{further
  parameters are handed over to
  \code{\link[hyperSpec]{read.txt.wide}}}
}
\value{
  hyperSpec object
}
\description{
  Read ASCII (.txt) files exported by Horiba's Labspec
  software (LabRAM spectrometers)

  \code{read.txt.Horiba.t} reads time series, i.e. .txt
  files with the time in the first column
}
\details{
  \code{read.txt.Horiba.xy} reads maps, i.e. .txt files
  where the first two columns give x and y coordinates.
}
\author{
  C. Beleites
}

