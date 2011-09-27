\name{mean_sd}
\alias{mean_sd,numeric-method}
\alias{mean_sd}
\title{Mean and Standard Deviation...}
\usage{\S4method{mean_sd}{numeric}(x, na.rm=TRUE, ...)

\S4method{mean_sd}{matrix}(x, na.rm=TRUE, ...)


\S4method{mean_sd}{hyperSpec}(x, na.rm=TRUE, ..., short="mean_sd", user=NULL, date=NULL)


\S4method{mean_pm_sd}{numeric}(x, na.rm=TRUE, ...)


\S4method{mean_pm_sd}{matrix}(x, na.rm=TRUE, ...)


\S4method{mean_pm_sd}{hyperSpec}(x, na.rm=TRUE, ..., short="mean_sd", user=NULL, date=NULL)


\S4method{mean}{hyperSpec}(x, na.rm=TRUE, ..., short="mean", user=NULL, date=NULL)


\S4method{quantile}{hyperSpec}(x, probs=seq(0, 1, 0.25), na.rm=TRUE, names="num", ...,
    short="quantile", user=NULL, date=NULL)

}
\description{Mean and Standard Deviation
Calculate mean and standard deviation, and mean, mean \eqn{\pm}{+-} one
standard deviation, respectively.}
\details{\code{mean_sd,numeric-method}: These functions are provided for convenience.

}
\value{\code{mean_sd,numeric-method}: \code{mean_sd} returns a vector with two values (mean and standard
deviation) of \code{x}.

\code{mean_sd,matrix-method}: \code{mean_sd (matrix)} returns a matrix with the mean spectrum in the first row and the standard deviation in the 2nd.


\code{mean_sd,hyperSpec-method}: \code{mean_sd} returns a hyperSpec object with the mean spectrum in the first row and the standard deviation in the 2nd.


\code{mean_pm_sd,numeric-method}: \code{mean_pm_sd} returns a vector with 3 values: mean - 1 sd, mean, mean + 1 sd


\code{mean_pm_sd,matrix-method}: \code{mean_pm_sd (matrix)} returns a matrix containing mean - sd, mean, and mean + sd
rows.


\code{mean_pm_sd,hyperSpec-method}: For hyperSpec objects, \code{mean_pm_sd} returns a hyperSpec object containing mean - sd,
mean, and mean + sd spectra.


\code{mean,hyperSpec-method}: For hyperSpec object, \code{mean} returns a hyperSpec object containing the mean
spectrum.


\code{quantile,hyperSpec-method}: For hyperSpec object, \code{quantile} returns a hyperSpec object containing the
respective quantile spectra.


}
\seealso{\code{\link[base]{mean}}, \code{\link[stats]{sd}}\code{\link[base]{mean}}, \code{\link[stats]{sd}}\code{\link[stats]{quantile}}}
\keyword{multivar}
\arguments{\item{x}{a numeric vector}
\item{na.rm}{handed to \code{\link[base]{mean}} and \code{\link[stats]{sd}}}
\item{\dots}{ignored (needed to make function generic)}
\item{short,user,date}{handed to \code{\link{logentry}}.}
\item{probs}{the quantiles, see \code{\link[stats]{quantile}}}
\item{names}{\code{"pretty"} results in percentages (like \code{\link[stats]{quantile}}'s
\code{names = TRUE}), \code{"num"} results in the row names being \code{as.character (probs)}
(good for ggplot2 getting the order of the quantiles right). Otherwise, no names are assigned.}
}
\examples{
mean_sd (flu [,, 405 ~ 410])

mean_sd (flu$spc)

mean_sd (flu)

mean_pm_sd (flu$c)

mean_pm_sd (flu$spc)

mean_pm_sd (flu)

plot (mean (chondro))

plot (quantile (chondro))
}
\alias{mean_sd,matrix-method}
\alias{mean_sd}
\keyword{multivar}
\alias{mean_sd,hyperSpec-method}
\alias{mean_sd}
\author{C. Beleites}
\keyword{univar}
\alias{mean_pm_sd,numeric-method}
\alias{mean_pm_sd}
\alias{mean_pm_sd}
\alias{mean_pm_sd,matrix-method}
\alias{mean_pm_sd}
\alias{mean_pm_sd,hyperSpec-method}
\alias{mean_pm_sd}
\alias{mean,hyperSpec-method}
\alias{mean}
\alias{quantile,hyperSpec-method}
\alias{quantile}

