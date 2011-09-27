\name{textio}
\alias{read.txt.long}
\title{Import and Export of hyperSpec objects...}
\usage{read.txt.long(file=stop("file is required"), cols=list(.wavelength =
    expression(lambda/nm), spc = "I / a.u."), header=TRUE, ...)

read.txt.wide(file=stop("file is required"), cols=list(spc = "I / a.u.", .wavelength
    = expression(lambda/nm)), check.names=FALSE, ...)


write.txt.long(object, file=stop("file is required"), order=c(".rownames",
    ".wavelength"), na.last=TRUE, decreasing=FALSE, quote=FALSE,
    sep="\t", row.names=FALSE, cols=NULL, col.names=TRUE,
    col.labels=FALSE, append=FALSE, ...)


write.txt.wide(object, file=stop("file is required"), cols=NULL, quote=FALSE,
    sep="\t", row.names=FALSE, col.names=TRUE, header.lines=1,
    col.labels=if (header.lines == 1) FALSE else TRUE, append=FALSE,
    ...)

}
\description{Import and Export of hyperSpec objects
Besides \code{\link[base]{save}} and \code{\link[base]{load}}, two general
ways to import and export data into \code{hyperSpec} objects exist.}
\details{\code{read.txt.long}: Firstly, hyperSpec objects can be imported and exported as ASCII files.

A second option is using the package \code{\link[R.matlab]{R.matlab}} which
provides the functions \code{\link[R.matlab]{readMat}} and
\code{\link[R.matlab]{writeMat}}.

hyperSpec comes with a number of pre-defined functions to import
manufacturer specific file formats. For details, see \code{vignette
("file-io")}.

\code{\link[hyperSpec]{read.spc}} imports Thermo Galactic's .spc file
format, and ENVI files may be read using
\code{\link[hyperSpec]{read.ENVI}}.

These functions are very flexible and provide lots of arguments.

If you use them to read or write manufacturer specific ASCII formats,
please consider writing a wrapper function and contributing this function
to \pkg{hyperSpec}.  An example is in the \dQuote{flu} vignette (see
\code{vignette ("flu", package = "hyperSpec"}).

Note that R accepts many packed formats for ASCII files, see
\code{\link[base]{connections}}. For .zip files, see
\code{\link[utils]{unzip}}.

For further information, see the examples below and the documentation of
\code{\link[R.matlab]{R.matlab}}.

\code{read.txt.wide}: Besides \code{\link[base]{save}} and \code{\link[base]{load}}, two general ways to import and
export data into \code{hyperSpec} objects exist.

Firstly, hyperSpec objects can be imported  and exported as ASCII files.

A second option is using the package \code{\link[R.matlab]{R.matlab}}
which provides the functions \code{\link[R.matlab]{readMat}} and
\code{\link[R.matlab]{writeMat}}.

hyperSpec comes with a number of pre-defined functions to import
manufacturer specific file formats. For details, see \code{vignette
("fileio")}.

\code{\link[hyperSpec]{read.spc}} imports Thermo Galactic's .spc file
format, and ENVI files may be read using
\code{\link[hyperSpec]{read.ENVI}}.  

These functions are very flexible and provide lots of arguments. 

If you use them to read or write manufacturer specific ASCII formats,
please consider writing a wrapper function and contributing this
function to \pkg{hyperSpec}.  An example is in the \dQuote{flu} vignette
(see \code{vignette ("flu", package = "hyperSpec"}).

Note that R accepts many packed formats for ASCII files, see
\code{\link[base]{connections}}. For .zip files, see \code{\link[utils]{unzip}}.

For further information, see the examples below, \code{vignette ("fileio")} and the documentation
of \code{\link[R.matlab]{R.matlab}}.


}
\alias{read.txt.long}
\alias{import}
\alias{export}
\author{C. Beleites}
\seealso{\code{\link[utils]{read.table}} and
\code{\link[utils]{write.table}}

\code{\link[R.matlab]{R.matlab}} for .mat files

\code{\link[hyperSpec]{read.ENVI}} for ENVI data

\code{\link[hyperSpec]{read.spc}} for .spc files

Manufacturer specific file formats: \code{\link{scan.txt.Renishaw}}\code{vignette ("fileio")} and \url{http://hyperspec.r-forge.r-project.org/fileio.pdf},
respectively}
\keyword{IO}
\keyword{file}
\arguments{\item{file}{filename or connection}
\item{cols}{the column names specifying the column order.

For data import, a list with elements \code{colname = label}; for export a
character vector with the colnames.  Use \code{wavelength} to specify the
wavelengths.}
\item{header}{the file has (shall have) a header line}
\item{\dots}{arguments handed to \code{\link[utils]{read.table}} and
\code{\link[utils]{write.table}}, respectively.}
\item{decreasing}{logical vector giving the sort order}
\item{check.names}{handed to \code{\link[utils]{read.table}}. Make sure
this is \code{FALSE}, if the column names of the spectra are the
wavelength values.}
\item{object}{the \code{hyperSpec} object}
\item{order}{which columns should be \code{\link[base]{order}}ed?
\code{order} is used as index vector into a \code{data.frame} with
columns given by \code{cols}.}
\item{na.last}{handed to \code{\link[base]{order}} by
\code{write.txt.long}.}
\item{quote,sep,row.names,col.names}{have their usual meaning (see
\code{\link[utils]{read.table}} and \code{\link[utils]{write.table}}),
but different default values.}
\item{col.labels}{Should the column labels be used rather than the
colnames?}
\item{append}{Should the output be appended to an existing file?}
\item{header.lines}{Toggle one or two line header (wavelengths in the
second header line) for \code{write.txt.wide}}
}
\examples{
\dontrun{vignette  ("file-io")}

## export & import matlab files
if (require (R.matlab)){
# export to matlab file
writeMat ("test.mat", x = flu[[]], wavelength = flu@wavelength,
label = lapply (flu@label, as.character))

# reading a matlab file
data <- readMat ("test.mat")
print (data)
mat <- new ("hyperSpec", spc = data$x,
wavelength = as.numeric(data$wavelength),
label = data$label[,,1])
}

## ascii export & import


write.txt.long (flu,  file = "flu.txt", cols = c(".wavelength", "spc", "c"), 
order = c("c", ".wavelength"),
decreasing = c(FALSE, TRUE))

read.txt.long (file = "flu.txt", cols = list (.wavelength = expression (lambda / nm), 
spc= "I / a.u", c = expression ("/" (c, (mg/l)))))

write.txt.wide (flu,  file = "flu.txt", cols = c("c", "spc"), 
col.labels = TRUE, header.lines = 2, row.names = TRUE)

write.txt.wide (flu,  file = "flu.txt", col.labels = FALSE, row.names = FALSE)

read.txt.wide (file = "flu.txt", 
cols = list (c=expression ("/"("c", "mg/l")), spc="I / a.u", .wavelength = "lambda / nm"),
header = TRUE)
}
\alias{read.txt.wide}
\alias{read.txt.wide}
\alias{write.txt.long}
\alias{write.txt.long}
\alias{write.txt.wide}
\alias{write.txt.wide}

