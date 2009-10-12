\name{plotc}
\alias{plotc}
\title{Calibration- and Timeseries Plots, Depth-Profiles and the like}
\description{
  \code{plotc} plots a summary value of each spectrum of a
  \code{hyperSpec} object over another dimension such as concentration,
  time, or a spatial coordinate.
}
\usage{
plotc(object, use.c = "c", func = sum, \dots,
      z = NULL, zlab = NULL, add = FALSE, plot.args = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ the \code{hyperSpec} object}
  \item{use.c}{the name of the column to plot against}
  \item{func}{function to compute the summary value from the spectra}
  \item{\dots}{further arguments to \code{func}}
  \item{z}{as an alternative to computing the value via \code{func}:
	 a numeric with the respective value for each of the spectra.}
  \item{zlab}{label for the ordinate}
  \item{add}{if FALSE, a new plot is created, if TRUE the output is
	 added to the active plot}
  \item{plot.args}{list with further arguments to \code{\link[graphics]{plot}} and
	 \code{\link[graphics]{lines}}, respectively.}
}
\details{
  If \code{z} is not given, it is calculated by \code{apply (object, 1,
	 func, \dots)}.
}
\author{C. Beleites}
\examples{

## example 1: calibration of fluorescence 
plotc (flu)

plotc (flu[,,450], zlab = expression (I ["450 nm"] / a.u.))

calibration <- lm (spc ~ c, data = flu[,,450]$.)
summary (calibration)

conc <- list (c = seq (from = 0.04, to = 0.31, by = 0.01))
ci <- predict (calibration, newdata = conc, interval = "confidence", level = 0.999) 
matlines (conc$c, ci, col = c ("red", "#606060","#606060"), lty = 1)



## example 2: time-trace of laser emission modes
cols <- c ("black", "blue", "#008000", "red")
wl <- i2wl (laser, c(13, 17, 21, 23))

plotspc (laser, axis.args=list (x = list (at = seq (404.5, 405.8, .1))))
for (i in seq_along (wl))
   abline (v = wl[i], col = cols[i], lwd = 2)

X11()
plotc (laser[,,wl [1]], use.c = "t",
       plot.args = list (ylim = range(laser[[]]), type = "o"))
for (i in seq_along (wl)[-1]){
   plotc (laser[,,wl[i]], use.c = "t", add = TRUE,
          plot.args = list (type = "o", col = cols[i]))
}

}
\keyword{hplot}

