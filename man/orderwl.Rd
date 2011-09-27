\name{orderwl}
\alias{orderwl}
\title{Sorting the Wavelengths of a hyperSpec Object...}
\usage{orderwl(x, na.last=TRUE, decreasing=FALSE, short="orderwl", date=NULL,
    user=NULL)
}
\description{Sorting the Wavelengths of a hyperSpec Object
Rearranges the \code{hyperSpec} object so that the wavelength vector is in increasing (or
decreasing) order.}
\details{The wavelength vector is sorted and the columns of the spectra matrix are rearranged accordingly.}
\value{A \code{hyperSpec} object.}
\author{C. Beleites}
\seealso{\code{\link[base]{order}}}
\arguments{\item{x}{The \code{hyperSpec} object.}
\item{na.last,decreasing}{Handed to \code{\link[base]{order}}.}
\item{short,date,user}{Handed to \code{\link{logentry}}.}
}
\examples{
## Example 1: different drawing order in plotspc
spc <- new ("hyperSpec", spc = matrix (rnorm (5) + 1:5, ncol = 5))
spc <- cbind (spc, spc+.5)

plot (spc, "spc")
text (wl (spc), spc [[]], as.character (1:10))
spc <- orderwl (spc)
plot (spc, "spc")
text (wl (spc), spc [[]], as.character (1:10))

## Example 2
spc <- new ("hyperSpec", spc = matrix (rnorm (5)*2 + 1:5, ncol = 5))
spc <- cbind (spc, spc)

plot (seq_len(nwl(spc)), spc[[]], type = "b")
spc[[]]

spc <- orderwl (spc)
lines (seq_len(nwl(spc)), spc[[]], type = "l", col = "red")
spc[[]]
}

