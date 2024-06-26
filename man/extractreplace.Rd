% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R, R/replace.R
\docType{methods}
\name{[,hyperSpec-method}
\alias{[,hyperSpec-method}
\alias{[}
\alias{[[,hyperSpec-method}
\alias{[[}
\alias{$,hyperSpec-method}
\alias{$}
\alias{[<-}
\alias{[<-,hyperSpec-method}
\alias{[[<-}
\alias{[[<-,hyperSpec-method}
\alias{$<-}
\alias{$<-,hyperSpec-method}
\title{Extract and Replace parts of hyperSpec objects}
\usage{
\S4method{[}{hyperSpec}(x, i, j, l, ..., wl.index = FALSE, drop = FALSE)

\S4method{[[}{hyperSpec}(x, i, j, l, ..., wl.index = FALSE, drop = FALSE)

\S4method{$}{hyperSpec}(x, name)

\S4method{[}{hyperSpec}(x, i, j, \dots) <- value

\S4method{[[}{hyperSpec}(x, i, j, l, wl.index = FALSE, \dots) <- value

\S4method{$}{hyperSpec}(x, name) <- value
}
\arguments{
\item{x}{a \code{hyperSpec} Object}

\item{i}{row index: selects spectra

\code{[[} and \code{[[<-} accept indexing with logical matrix or a n by 2
  integer index matrix. In this case the indexing is done inside the
  spectra matrix. See the examples below.}

\item{j}{selecting columns of \code{x@data}}

\item{l}{selecting columns of the spectra matrix. If \code{l} is numeric,
the default behaviour is treating \code{l} as wavelengths, \emph{not} as
indices.}

\item{...}{ignored}

\item{wl.index}{If \code{TRUE} (default), the value(s) in \code{l} are
treated as column indices for the spectral matrix. Otherwise, the numbers
in \code{l} are treated as wavelengths and the corresponding column
indices are looked up first via \code{\link{wl2i}}.}

\item{drop}{For \code{[[}: drop unnecessary dimensions, see
\code{\link[base]{drop}} and \code{\link[base]{Extract}}. Ignored for
\code{[}, as otherwise invalid \code{hyperSpec} objects might result.}

\item{name}{name of the data column to extract. \code{$spc} yields the spectra matrix.}

\item{value}{the replacement value}
}
\value{
For \code{[}, \code{[<-}, \code{[[<-}, and \code{$<-} a \code{hyperSpec} object,

for \code{[[} a matrix or \code{data.frame}, and

for \code{$} the column of the \code{data.frame} \code{@data}.

\code{x[[]]} returns the complete spectra matrix.

\code{x$.} returns the complete slot \code{@data},

\code{x$..} returns the \code{data.frame} in \code{@data} but without the column
\code{@data$spc} containing the spectra matrix.
}
\description{
These Methods allow to extract and replace parts of the \code{hyperSpec} object.
}
\details{
They work with respect to the spectra (rows of \code{x}), the columns of the data matrix, and the
wavelengths (columns of the spectra matrix).

Thus, they can be used for selecting/deleting spectra, cutting the spectral range, and extracting
or setting the data belonging to the spectra.

Convenient shortcuts for access of the spectra matrix and the \code{data.frame} in slot
\code{data} are provided.

\emph{Extracting: \code{[}, \code{[[}, and \code{$}}.

The version with single square brackets (\code{[}) returns the resulting \code{hyperSpec} object.

\code{[[} yields \code{data.frame} of slot \code{@data} of that corresponding \code{hyperSpec}
object returned with the same arguments by \code{[} if columns were selected (i.e. \code{j} is
given), otherwise the spectra \code{matrix} \code{x@data$spc}.

\code{$} returns the selected column of the \code{data.frame} in slot \code{@data}.

\emph{Shortcuts.} Three shortcuts to conveniently extract much needed parts of the object are
defined:

\code{x[[]]} returns the spectra matrix.

\code{x$.} returns the complete slot \code{@data}, including the spectra matrix in column
\code{$spc}, as a \code{data.frame}.

\code{x$..} returns a \code{data.frame} like \code{x$.} but without the spectra matrix.

\emph{Replacing: \code{[<-}, \code{[[<-}, and \code{$<-}}.
\preformatted{
## S4 method for signature 'hyperSpec':
x [i, j, l, \dots] <- value

## S4 method for signature 'hyperSpec':
x [[i, j, l, wl.index = FALSE, \dots]] <- value

## S4 method for signature 'hyperSpec':
x$name <- value
}

\code{value} gives the values to be assigned.\cr

For \code{$}, this can also be a list of the form \code{list (value =
value, label = label)}, with \code{label} containing the label for data
column \code{name}.

\code{[[<-} replaces parts of the spectra matrix.

\code{[<-} replaces parts of the \code{data.frame} in slot \code{x@data}.

\code{$<-} replaces a column of the \code{data.frame} in slot
\code{x@data}.  The \code{value} may be a list with two elements,
\code{value} and \code{label}.  In this case the label of the data column
is changed accordingly.

\code{$..<-} is again an abbreviation for the data.frame without the
spectra matrix.
}
\examples{

## index into the rows (spectra) -------------------------------------
## make some "spectra"

## numeric index
plot (flu, "spc", lines.args = list (lty = 2))
plot (flu[1:3], "spc", add = TRUE, col = "red")     # select spectra
plot (flu[-(1:3)], "spc", add = TRUE, col = "blue") # delete spectra

## logic index
plot (flu, "spc", lines.args = list (lty = 2))
index <- rnorm (6) > 0
index
plot (flu[index], "spc", add = TRUE, col = "red")   # select spectra
plot (flu[!index], "spc", add = TRUE, col = "blue") # select spectra

## index into the data columns ---------------------------------------
range (chondro[[,"x"]])
colnames (chondro[[,1]])
dim (chondro[[,c(TRUE, FALSE, FALSE)]])
chondro$x


## the shortcut functions --------------------------------------------

## extract the spectra matrix
flu[[]]

## indexing via logical matrix
summary (flu [[flu < 125]])

## indexing the spectra matrix with index matrix n by 2
ind <- matrix (c (1, 2, 4, 406, 405.5, 409), ncol = 2)
ind
flu [[ind]]

ind <- matrix (c (1, 2, 4, 4:6), ncol = 2)
ind
flu [[ind, wl.index = TRUE]]

pca <- prcomp (flu[[]])

## result is data.frame, if j is given:
result <- flu [[, 1:2, 405 ~ 410]]
result
class (result)
colnames (result)

## extract the data.frame including the spectra matrix
flu$.
dim(flu$.)
colnames (flu$.)
flu$.$spc

calibration <- lm (spc ~ c, data = flu[,,450]$.)
calibration

flu$..
colnames (flu$..)

## replacement functions
spc <- flu
spc$.
spc[, "c"] <- 16 : 11
## be careful:
plot (spc)
spc [] <- 6 : 1
spc$..
plot (spc)

spc <- flu [,, 405 ~ 410]
spc [[]]
spc [[3]] <- -spc[[3]]
spc [[]]
spc [[,,405 : 410]] <- -spc[[,,405 : 410]]
spc [[]]
spc [[,,405 ~ 410]] <- -spc[[,,405 ~ 410]]

## indexing with logical matrix
spc <- flu [,, min ~ 410]
spc < 125
spc [[spc < 125]] <- NA
spc [[]]

## indexing with n by 2 matrix
ind <- matrix (c (1, 2, 4, 406, 405.5, 409), ncol = 2)
ind
spc [[ind]] <- 3
spc [[]]

ind <- matrix (c (1, 2, 4, 4:6), ncol = 2)
ind
spc [[ind, wl.index = TRUE]] <- 9999
spc [[]]

spc$.
spc$z <- 1 : 6
spc
spc$z <- list (1 : 6, "z / a.u.")

}
\seealso{
\code{\link{wl2i}} on conversion of wavelength ranges to indices.

\code{\link[base]{drop}} and \code{\link[base]{Extract}} on \code{drop}.
}
\keyword{manip}
\keyword{methods}
