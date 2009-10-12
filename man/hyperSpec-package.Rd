\name{hyperSpec-package}
\Rdversion{1.1}
\alias{hyperSpec-package}
\docType{package}
\title{
Interface for hyperspectral data sets, i.e. spectra + meta information (spatial, time, concentration, ...)
}
\description{
This package gives an interface to handle hyperspectral data sets in R. I.e. spatially or time-resolved spectra, or spectra with any other kind of information associated with the spectra. 

The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES, NIR, IR, Raman, NMR, etc. 

More generally, any data that is recorded over a discretized variable, e.g. absorbance = f (wavelength), stored as a vector of absorbance values for discrete wavelengths is suitable.
}
\details{
\tabular{ll}{
Package: \tab hyperSpec\cr
Type: \tab Package\cr
Version: \tab 0.95\cr
Date: \tab 2009-10-12\cr
License: \tab LGPL\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
Depends: \tab methods, utils, lattice\cr
#Imports: \tab lattice\cr
Suggests: \tab R.matlab\cr
Packaged: \tab 2009-10-12 13:50:19 UTC; cb\cr
Built: \tab R 2.9.2; ; 2009-10-12 14:55:39 UTC; unix\cr
}

Index:
\preformatted{
Summary,hyperSpec-method
                        Math Functions for hyperSpec Objects
[,hyperSpec-method      Extract and Replace Methods for hyperSpec
aggregate,hyperSpec-method
                        aggregate
apply,hyperSpec-method
                        apply
array2df                array2df: Convert multidimensional array into
                        matrix or data.frame
array2vec               Converting array and vector Indices
as.data.frame-methods   Conversion of a hyperSpec object into a
                        data.frame or matrix
bind                    Binding hyperSpec Objects
chondro                 Raman spectra of 2 Chondrocytes in Cartilage
decomposition           Convert Principal Component Decomposition or
                        the like into a hyperSpec Object
dimnames,hyperSpec-method
                        Dimnames of an hyperSpec Object
flu                     Chitin Fluorescence Spectra
hyperSpec               Interface for hyperspectral data sets
hyperSpec Arith         Arithmetical Operators: +, -, *, /, ^, \%\%,
                        \%/\%, \%*\%
hyperSpec Comparison    Comparison Operators: <, >, <=, >=, ==, and !=
hyperSpec Math          Math Functions for hyperSpec Objects
hyperSpec-class         Class "hyperSpec"
hyperSpec-package       Interface for hyperspectral data sets, i.e.
                        spectra + meta information (spatial, time,
                        concentration, ...)
import                  Import and Export of hyperSpec objects
index.grid              index.grid: Calculate a Matrix with Indices for
                        x,y-Data
initialize,hyperSpec-method
                        Creating a hyperSpec Object
labels,hyperSpec-method
                        Get and Set Labels of a hyperSpec Object
laser                   Laser Emission
logbook                 Logging the processing ot a hyperSpec Object
logentry                Append a Row to the log of a hyperSpec Object
matlab.palette          Matlab-like Palette
mean_sd                 Mean and Standard Deviation
ncol,hyperSpec-method   The Number of Rows (Spectra), Columns, and Data
                        Points per Spectrum of an hyperSpec Object)
orderwl                 Sorting the Wavelengths of a hyperSpec Object
pearson.dist            Calculate Distance based on Pearson's R\^2
plot                    Plotting hyperSpec Objects
plotc                   Calibration- and Timeseries Plots,
                        Depth-Profiles and the like
plotmap                 Plot a Map and Identify Spectra
plotspc                 Plotting Spectra
scan.txt.Renishaw       import Raman measurements from Renishaw
                        ASCII-files
show,hyperSpec-method   Convert a hyerSpec object to character strings
                        for Display
spc.bin                 Wavelength Binning
spc.fit.poly            Polynomial Baseline Fitting
spc.identify            Identifying Spectra and Spectral Data Points
spc.loess               loess smoothing interpolation for spectra
split-methods           Split a hyperSpec object into groups
stacked.offsets         y Offsets for Stacked Plots
sweep                   Sweep Summary Statistic out of an hyperSpec
                        Object
wc                      wc
wl                      Getting and Setting the Wavelength Axis
wl2i                    Conversion between Wavelength and Spectra
                        Matrix Column Index
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{baselinebelow} \tab baselinebelow (source, pdf)\cr
\code{chondro} \tab Vignette on the chondro data set. Shows common Preprocessing tasks for Raman spectra, principal component analysis (PCA), and hierarchical cluster analysis. (source, pdf)\cr
\code{flu} \tab flu: Vignette on the fluorescence data set, how to write an import function, and example of a linear calibration (source, pdf)\cr
\code{introduction} \tab hyperSpec Introduction (source, pdf)\cr
\code{laser} \tab laser: Vignette on the laser data set, example of a time series. Shows also how to change the spectral abscissa. (source, pdf)\cr
}
}
\author{C. Beleites

Maintainer: Claudia Beleites <cbeleites@units.it>
}
\keyword{ package }
