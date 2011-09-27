\name{plot}
\alias{plot-methods,-method}
\alias{plot-methods}
\title{Plotting hyperSpec Objects...}
\description{Plotting hyperSpec Objects}
\details{\code{plot-methods,-method}: Plotting \code{hyperSpec} objects. The \code{plot} method for
\code{hyperSpec} objects is a switchyard to \code{\link{plotspc}},
\code{\link{plotmap}}, and \code{\link{plotc}}.

It also supplies some convenient abbrevations for much used plots.

If \code{y} is missing, \code{plot} behaves like \code{plot (x, y =
"spc")}.

Supported values for \code{y} are:

\describe{ \item{"spc"}{calls \code{\link{plotspc}} to produce a spectra
plot.}

\item{"spcmeansd"}{plots mean spectrum +/- one standard deviation}

\item{"spcprctile"}{plots 16th, 50th, and 84th percentile spectre. If the
distributions of the intensities at all wavelengths were normal, this would
correspond to \code{"spcmeansd"}. However, this is frequently not the case.
Then \code{"spcprctile"} gives a better impression of the spectral data
set.}

\item{"spcprctl5"}{like \code{"spcprctile"}, but additionally the 5th and
95th percentile spectra are plotted.}

\item{"map"}{calls \code{\link{plotmap}} to produce a map plot.}

\item{"c"}{calls \code{\link{plotc}} to produce a calibration (or time
series, depth-profile, or the like)}

\item{"ts"}{plots a time series: abbrevation for \code{\link{plotc} (x,
use.c = "t")}}

\item{"depth"}{plots a depth profile: abbrevation for \code{\link{plotc}
(x, use.c = "z")}} }

}
\alias{plot}
\alias{plot,ANY,ANY-method}
\alias{plot,hyperSpec,character-method}
\alias{plot,hyperSpec,missing-method}
\docType{methods}
\author{C. Beleites}
\seealso{\code{\link{plotspc}} for spectra plots (intensity over
wavelength),

\code{\link{plotmap}} for plotting maps, i.e. color coded summary value on
two (usually spatial) dimensions.

\code{\link{plotc}}

\code{\link[graphics]{plot}}}
\keyword{methods}
\keyword{hplot}
\arguments{\item{x}{the \code{hyperSpec} object}
\item{y}{selects what plot should be produced}
\item{\dots}{arguments passed to the respective plot function}
}
\examples{
plot (flu)

plot (flu, "c")

plot (laser, "ts")

spc <- apply (chondro, 2, quantile, probs = 0.05)
spc <- sweep (chondro, 2, spc, "-")
plot (spc, "spcprctl5")
plot (spc, "spcprctile")
plot (spc, "spcmeansd")
}
\alias{plot,-method}
\alias{plot}

