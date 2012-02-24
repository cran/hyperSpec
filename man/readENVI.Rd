\name{read.ENVI.Nicolet}
\alias{read.ENVI}
\alias{read.ENVI.Nicolet}
\title{Nicolet uses some more keywords in their header file.
\code{read.ENVI.Nicolet} therefore appends "description", "z plot titles",
and "pixel size" to \code{keys.hdr2log} before calling \code{read.ENVI}.
They are then interpreted as follows:
\tabular{ll}{
description   \tab giving the position of the first spectrum \cr
z plot titles \tab wavelength and intensity axis units, comma separated \cr
pixel size    \tab interpreted as x and y step size
}}
\usage{
  read.ENVI.Nicolet(..., x = NA, y = NA, log = list(),
    keys.hdr2log = TRUE, nicolet.correction = FALSE)

  read.ENVI(file = stop("read.ENVI: file name needed"),
    headerfile = NULL, header = list(),
    keys.hdr2data = FALSE, keys.hdr2log = TRUE, x = 0:1,
    y = x, wavelength = NULL, label = list(), log = list())
}
\arguments{
  \item{nicolet.correction}{see details}

  \item{\dots}{handed to \code{read.ENVI}}

  \item{file}{complete name of the binary file}

  \item{headerfile}{name of the ASCII header file. If
  \code{NULL}, the name of the header file is guessed by
  looking for a second file with the same basename but
  different suffix as \code{file}.}

  \item{header}{list with the respective information, see
  details.}

  \item{x,y}{vectors of form c(offset, step size) for the
  position vectors, see details.}

  \item{wavelength,label,log}{lists that overwrite the
  respective information from the ENVI header file. These
  data is then handed to
  \code{\link[hyperSpec]{initialize}}}

  \item{keys.hdr2data,keys.hdr2log}{determine which fields
  of the header file should be put into the extra data and
  which are to be stored in \code{long.description$header}
  in the hyperSpec object's log. Defaults to putting all
  header information into the log, and none as extra data.

  To specify certain entries, give character vectors
  containing the lowercase names of the header file
  entries.}
}
\value{
  a \code{hyperSpec} object
}
\description{
  The values in header line description seem to be microns
  while the pixel size seems to be in microns. If
  \code{nicolet.correction} is true, the pixel size values
  (i.e. the step sizes) are multiplied by 1000.

  \code{read.ENVI.Nicolet} should be a good starting point
  for writing custom wrappers for \code{read.ENVI} that
  take into account your manufacturer's special entries in
  the header file.
}
\details{
  ENVI data usually consists of two files, an ASCII header
  and a binary data file. The header contains all
  information necessary for correctly reading the binary
  file.

  I experienced missing header files (or rather: header
  files without any contents) produced by Bruker Opus' ENVI
  export.

  In this case the necessary information can be given as a
  list in parameter \code{header} instead. The elements of
  header are then:

  \tabular{lll}{ \code{header\$} \tab values \tab
  meaning\cr \code{samples} \tab integer \tab no of columns
  / spectra in x direction\cr \code{lines} \tab integer
  \tab no of lines / spectra in y direction\cr \code{bands}
  \tab integer \tab no of wavelengths / data points per
  spectrum\cr \code{`data type`} \tab \tab format of the
  binary file\cr \tab 1 \tab 1 byte unsigned integer \cr
  \tab 2 \tab 2 byte signed integer \cr \tab 3 \tab 4 byte
  signed integer \cr \tab 4 \tab 4 byte float \cr \tab 5
  \tab 8 byte double \cr \tab 9 \tab 16 (2 x 8) byte
  complex double \cr \tab 12 \tab 2 byte unsigned integer
  \cr \code{`header offset`} \tab integer \tab number of
  bytes to skip before binary data starts\cr
  \code{interleave} \tab \tab directions of the data cube
  \cr \tab "BSQ" \tab band sequential (indexing: [sample,
  line, band])\cr \tab "BIL" \tab band interleave by line
  (indexing: [sample, line, band])\cr \tab "BIP" \tab band
  interleave by pixel (indexing: [band, line, sample])\cr
  \code{`byte order`} \tab 0 or "little" \tab little endian
  \cr \tab 1 or "big" \tab big endian \cr \tab "swap" \tab
  swap byte order }

  Some more information that is not provided by the ENVI
  files may be given:

  Wavelength axis and axis labels in the respective
  parameters. For more information, see
  \code{\link[hyperSpec]{initialize}}.

  The spatial information is by default a sequence from 0
  to \code{header$samples - 1} and \code{header$lines - 1},
  respectively. \code{x} and \code{y} give offset of the
  first spectrum and step size.

  Thus, the object's \code{$x} colum is: \code{(0 :
  header$samples - 1) * x [2] + x [1]}.  The \code{$y}
  colum is calculated analogously.
}
\author{
  C. Beleites, testing for the Nicolet files C. Dicko
}
\references{
  This function was adapted from
  \code{\link[caTools]{read.ENVI}}:

  Jarek Tuszynski (2008). caTools: Tools: moving window
  statistics, GIF, Base64, ROC AUC, etc.. R package version
  1.9.
}
\seealso{
  \code{\link[caTools]{read.ENVI}}

  \code{\link[hyperSpec]{textio}}
}
\keyword{file}
\keyword{IO}

