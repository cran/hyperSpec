\name{read-spc}
\alias{read.spc.KaiserMap}
\title{read.spc.KaiserMap}
\usage{read.spc.KaiserMap(files, keys.hdr2data=FALSE, keys.hdr2log=TRUE, keys.log2data=NULL,
    keys.log2log=TRUE, glob=TRUE, ...)

read.spc(filename, keys.hdr2data=c("fexper", "fres", "fsource"),
    keys.hdr2log=c("fdate", "fpeakpt"), keys.log2data=FALSE,
    keys.log2log=TRUE, log.txt=TRUE, log.bin=FALSE, log.disk=FALSE,
    hdr=list(), no.object=FALSE)

}
\description{\code{read.spc.KaiserMap} imports sets of .spc files of Raman maps written
by Kaiser Optical Systems' Hologram software.  It may also serve as an
example how to write wrapper functions for \code{read.spc} to conveniently
import specialized sets of .spc files.}
\arguments{\item{glob}{If \code{TRUE} the filename is interpreted as a wildcard
containing file name pattern and expanded to all matching file names.}
\item{\dots}{All further arguments to \code{read.spc.KaiserMap} are handed
over directly to \code{read.spc}.}
\item{filename}{The complete file name of the .spc file.}
\item{files}{If \code{glob = TRUE}, \code{filename} can contain wildcards.
Thus all files matching the name pattern in \code{filename} can be
specified.}
\item{keys.hdr2data,keys.hdr2log,keys.log2data,keys.log2log}{character
vectors with the names of parameters in the .spc file's log block
(log2xxx) or header (hdr2xxx) that should go into the extra data
(yyy2data) or into the \code{long.description} field of the returned
hyperSpec object's log (yyy2log).

All header fields specified in the .spc file format specification (see
below) are imported and can be referred to by their de-capitalized names.}
\item{log.txt}{Should the text part of the .spc file's log block be read?}
\item{log.bin,log.disk}{Should the normal and on-disk binary parts of the
.spc file's log block be read?  If so, they will be put as raw vectors
into the hyperSpec object's log.}
\item{hdr}{A list with fileheader fields that overwrite the settings of
actual file's header.

Use with care, and look into the source code for detailed insight on the
elements of this list.}
\item{no.object}{If \code{TRUE}, a list with wavelengths, spectra, labels,
log and data are returned instead of a hyperSpec object.

This parameter will likely be subject to change in future - use with care.}
}
\alias{read.spc}
\value{\code{read.spc}: If the file contains multiple spectra with individual wavelength
axes, \code{read.spc} returns a list of hyperSpec objects.  Otherwise the
result is a hyperSpec object.

\code{read.spc.KaiserMap} returns a hyperSpec object with data columns x,
y, and z containing the stage position as recorded in the .spc files'
log.

}
\note{Only a restricted set of test files was available for development.
Particularly, the w-planes feature could not be tested.

If you have .spc files that cannot be read with these function, don't
hesitate to contact the package maintainer with your code patch or asking
advice.}
\author{C. Beleites}
\seealso{\code{\link[hyperSpec]{textio}}}
\references{Source development kit and file format specification of .spc
files:
\url{https://ftirsearch.com/features/converters/SPCFileFormat.htm}.}
\keyword{IO}
\keyword{file}
\examples{
## get the sample .spc files from ftirsearch.com (see above)
\dontrun{
# single spectrum
spc <- read.spc ("BENZENE.SPC")
plot (spc)

# multi-spectra .spc file with common wavelength axis
spc <- read.spc ('IG_MULTI.SPC')
spc

# multi-spectra .spc file with individual wavelength axes
spc <- read.spc ("BARBITUATES.SPC")
plot (spc [[1]], lines.args = list (type = "h"))
}
}

