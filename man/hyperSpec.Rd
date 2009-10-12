\name{hyperSpec}
\Rdversion{1.1}
\alias{hyperSpec}
\docType{package}
\title{
Interface for hyperspectral data sets
}
\description{
This package gives an interface to handle hyperspectral data sets in
R. Hyperspectral data are spatially or time-resolved spectra, or spectra with any other
kind of information associated with the spectra. E.g. spectral maps or
images, time series, calibration series, etc.

The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES,
NIR, IR, Raman, NMR, MS, etc.

More generally, any data that is recorded over a discretized variable,
e.g. absorbance = f (wavelength), stored as a vector of absorbance
values for discrete wavelengths is suitable.
}
\details{
The following vignettes should provide a start:
\tabular{ll}{
\code{introduction} \tab hyperSpec Introduction (source, pdf)\cr
\code{chondro} \tab Vignette on the chondro data set (Raman map). Shows common Preprocessing tasks for Raman spectra, principal component analysis (PCA), and hierarchical cluster analysis. (source, pdf)\cr
\code{flu} \tab flu: Vignette on a small fluorescence data set, how to write an import function, and example of a linear calibration (source, pdf)\cr
}
}
\seealso{
  \code{citation ("hyperSpec")} produces the correct citation.

  \code{package?hyperSpec} for information about the package
  
  \code{class?hyperSpec} for details on the S4 class provided by this package.
}

\author{C. Beleites

Maintainer: Claudia Beleites <cbeleites@units.it>
}
\keyword{ package }
