\name{read.ENVI}
\Rdversion{1.1}
\alias{read.ENVI}
\title{Import of ENVI data as hyperSpec object}
\description{
This function allows ENVI data import as \code{hyperSpec} object. 
}
\usage{
read.ENVI(file = stop ("read.ENVI: file name needed"), header = NULL,
          x = 0:1, y = x,
          wavelength = NULL, label = NULL, log = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{name of the binary file}
  \item{header}{name of the ASCII header file or list with the
    respective information, see details. If \code{NULL}, the name of the
    header file is guessed.}
  \item{x, y}{vectors of form c(offset, step size) for the position
    vectors, see details.}
  \item{wavelength, label, log}{handed to \code{\link[hyperSpec]{initialize}}}
}
\details{
  ENVI data usually consists of two files, an ASCII header and a binary data
  file. The header contains all information necessary for correctly reading the
  binary file.

  I experienced missing header files (or rather: header files
  without any contents) produced by Bruker Opus' ENVI export.
  
  In this case the necessary information can be given as a list in
  parameter \code{header} instead. The elements of header are then:

  \tabular{lll}{
    \code{header\$}        \tab values    \tab meaning\cr
    \code{samples}         \tab integer \tab no of columns / spectra in x direction\cr
    \code{lines}           \tab integer \tab no of lines / spectra in y direction\cr
    \code{bands}           \tab integer\tab no of wavelengths / data points per spectrum\cr
    \code{`data type`}     \tab \tab format of the binary file\cr
    \tab  1 \tab 1 byte unsigned integer \cr
    \tab  2 \tab 2 byte signed integer \cr
    \tab  3 \tab 4 byte signed integer \cr
    \tab  4 \tab 4 byte float \cr
    \tab  5 \tab 8 byte double \cr
    \tab  9 \tab 16 (2 x 8) byte complex double \cr
    \tab 12 \tab 2 byte unsigned integer \cr
    \code{`header offset`} \tab integer\tab number of bytes to skip before
    binary data starts\cr
    \code{interleave}      \tab       \tab directions of the data cube \cr
    \tab "BSQ" \tab band sequential (indexing: [sample, line, band])\cr
    \tab "BIL" \tab band interleave by line  (indexing: [sample, line, band])\cr
    \tab "BIP" \tab band interleave by pixel (indexing: [band, line, sample])\cr
    \code{`byte order`}    \tab 0 or "little" \tab little endian \cr
    \tab 1 or "big" \tab big endian \cr
    \tab "swap" \tab swap byte order 
  }

  Some more information that is not provided by the ENVI files may be
  given:
  
  Wavelength axis and axis labels in the respective parameters. For more
  information, see \code{\link[hyperSpec]{initialize}}.

  The spatial information is by default a sequence from 0 to
  \code{header$samples - 1} and \code{header$lines - 1}, respectively.
  \code{x} and \code{y} give offset of the first spectrum and step size.
  
  Thus, the object's \code{$x} colum is:
  \code{(0 : header$samples - 1) * x [2] + x [1]}.
  The \code{$y} colum is calculated analogously.
}
\value{
  a \code{hyperSpec} object
}
\references{
This function was adapted from \code{\link[caTools]{read.ENVI}}:
  
Jarek Tuszynski (2008). caTools: Tools: moving window statistics,
GIF, Base64, ROC AUC, etc.. R package version 1.9.
}
\author{C. Beleites}

\section{Warning }{I do only have ENVI filed saved by Opus / Bruker
  FT-IR FPA spectrometer (BIP format). Thus the function is not
  extensively tested. 
  
  If you import other ENVI data (successful and unsuccessful), please
  let me know (e-mail: <cbeleites@units.it>).
}

\seealso{
\code{\link[caTools]{read.ENVI}}
}
\keyword{IO}
\keyword{file}
