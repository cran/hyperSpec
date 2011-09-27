\name{hyperSpec-class}
\Rdversion{1.1}
\docType{class}
\alias{hyperSpec-class}

\title{Class "hyperSpec"}
\description{This class handles hyperspectral data sets, i.e. spatially or time-resolved spectra, or spectra with any other
kind of information associated with the spectra.

The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES,
NIR, IR, Raman, NMR, MS, etc.

More generally, any data that is recorded over a discretized variable, e.g. absorbance = f (wavelength), stored as a vector of absorbance values for discrete wavelengths is suitable.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("hyperSpec", spc,
  data, wavelength, label, log)}, for details see \code{\link[hyperSpec]{initialize}}.
}
\section{Slots}{
	 \describe{
    \item{\code{wavelength}:}{Numeric vector giving the wavelengths
      (or wavenumbers, frequencies, m/z, \dots) for
      each data point of the spectrum.
	
		\code{wavelength} wavelength axis of the spectra.
	 }
    \item{\code{data}:}{\code{data.frame} with the spectra and further
      data for each spectrum (e.g. x/y/z coordinates, times, sample
      numbers, concentrations, etc.).
		
      The spectra are stored in \code{data$spc}, preferrably as a matrix.
    }
    \item{\code{label}:}{List with the labels (character vectors or
		expressions) that should be used to describe the columns of
		\code{data}. 

		The label for \code{wavelength} is in \code{label$.wavelength}.
	 }
    \item{\code{log}:}{A \code{data.frame} with the columns
      \code{short.description}, \code{long.description}, \code{date},
      and \code{name} tracking what is done with the object.
	 }
  }
}
\section{Methods}{
  \describe{
    \item{aggregate}{\code{signature(x = "hyperSpec")}: ... }
    \item{all.equal}{\code{signature(target = "hyperSpec", current = "hyperSpec")}: ... }
    \item{apply}{\code{signature(X = "hyperSpec")}: ... }
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "hyperSpec")}: ... }
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "numeric")}: ... }
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "matrix")}: ... }
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "missing")}: ... }
    \item{Arith}{\code{signature(e1 = "numeric", e2 = "hyperSpec")}: ... }
    \item{Arith}{\code{signature(e1 = "matrix", e2 = "hyperSpec")}: ... }
    \item{as.character}{\code{signature(x = "hyperSpec")}: ... }
    \item{as.data.frame}{\code{signature(x = "hyperSpec", row.names = "missing", optional = "missing")}: ... }
    \item{as.matrix}{\code{signature(x = "hyperSpec")}: ... }
    \item{[<-}{\code{signature(x = "hyperSpec")}: ... }
    \item{[}{\code{signature(x = "hyperSpec")}: ... }
    \item{[[<-}{\code{signature(x = "hyperSpec")}: ... }
    \item{[[}{\code{signature(x = "hyperSpec")}: ... }
    \item{$<-}{\code{signature(x = "hyperSpec")}: ... }
    \item{$}{\code{signature(x = "hyperSpec")}: ... }
    \item{\%*\%}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: ... }
    \item{\%*\%}{\code{signature(x = "hyperSpec", y = "matrix")}: ... }
    \item{\%*\%}{\code{signature(x = "matrix", y = "hyperSpec")}: ... }
    \item{cbind2}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: ... }
    \item{cbind2}{\code{signature(x = "hyperSpec", y = "missing")}: ... }
    \item{colnames}{\code{signature(x = "hyperSpec")}: ... }
    \item{Compare}{\code{signature(e1 = "hyperSpec", e2 = "hyperSpec")}: ... }
    \item{Compare}{\code{signature(e1 = "hyperSpec", e2 = "numeric")}: ... }
    \item{Compare}{\code{signature(e1 = "hyperSpec", e2 = "matrix")}: ... }
    \item{Compare}{\code{signature(e1 = "numeric", e2 = "hyperSpec")}: ... }
    \item{Compare}{\code{signature(e1 = "matrix", e2 = "hyperSpec")}: ... }
    \item{dim}{\code{signature(x = "hyperSpec")}: ... }
    \item{dimnames}{\code{signature(x = "hyperSpec")}: ... }
    \item{initialize}{\code{signature(.Object = "hyperSpec")}: ... }
    \item{labels}{\code{signature(object = "hyperSpec")}: ... }
    \item{log}{\code{signature(x = "hyperSpec")}: ... }
    \item{Math2}{\code{signature(x = "hyperSpec")}: ... }
    \item{Math}{\code{signature(x = "hyperSpec")}: ... }
    \item{ncol}{\code{signature(x = "hyperSpec")}: ... }
    \item{nrow}{\code{signature(x = "hyperSpec")}: ... }
    \item{plot}{\code{signature(x = "hyperSpec", y = "missing")}: ... }
    \item{plot}{\code{signature(x = "hyperSpec", y = "character")}: ... }
    \item{print}{\code{signature(x = "hyperSpec")}: ... }
    \item{rbind2}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: ... }
    \item{rbind2}{\code{signature(x = "hyperSpec", y = "missing")}: ... }
    \item{rownames}{\code{signature(x = "hyperSpec")}: ... }
    \item{show}{\code{signature(object = "hyperSpec")}: ... }
    \item{split}{\code{signature(x = "hyperSpec")}: ... }
    \item{summary}{\code{signature(object = "hyperSpec")}: ... }
    \item{Summary}{\code{signature(x = "hyperSpec")}: ... }
    \item{sweep}{\code{signature(x = "hyperSpec")}: ... }
	 }
}
\author{C. Beleites}
\seealso{
  See the vignette "introduction" for an introduction to hyperSpec from
	 a spectroscopic point of view.
}
\examples{
showClass("hyperSpec")
\dontrun{vignette ("introduction")}
}
\keyword{classes}
