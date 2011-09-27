\name{asdataframe}
\alias{as.data.frame,hyperSpec,missing,missing-method}
\alias{as.data.frame}
\title{Conversion of a hyperSpec object into a data...}
\usage{\S4method{as.data.frame}{hyperSpec,missing,missing}(x, row.names=stop("hyperSpec method does not support row.names"),
    optional=stop("hyperSpec method does not support row.names"), ...)

\S4method{as.matrix}{hyperSpec}(x, ...)


as.wide.df(x)


as.long.df(x, rownames=FALSE, wl.factor=FALSE, na.rm=TRUE)


as.t.df(x)

}
\description{Conversion of a hyperSpec object into a data.frame or matrix
\code{as.data.frame} returns \code{x@data} (as data.frame) \code{as.matrix}
returns the spectra matrix \code{x@data$spc} as matrix}
\alias{as.data.frame}
\alias{as.data.frame-methods}
\alias{as.data.frame,hyperSpec,missing,missing-method}
\alias{as.data.frame,hyperSpec-method}
\docType{methods}
\value{\code{as.data.frame,hyperSpec,missing,missing-method}: \code{x@data} and \code{x@data$spc} (== \code{x$spc} == \code{x [[]]}), respectively.

\code{as.wide.df}: \code{as.wide.df} returns a data.frame that consists of the extra data and
the spectra matrix converted to a data.frame. The spectra matrix is
expanded \emph{in place}.


\code{as.long.df}: \code{as.long.df} returns the stacked or molten version of \code{x@data}. The
wavelengths are in column \code{.wavelength}.


\code{as.t.df}: \code{as.t.df} returns a data.frame similar to \code{as.long.df}, but each
spectrum in its own column. This is useful for exporting summary spectra,
see the example.


}
\author{C. Beleites}
\seealso{\code{\link[base]{as.data.frame}}and \code{\link[base]{as.matrix}}

\code{\link[hyperSpec:extractreplace]{[}} for a shortcut to \code{as.matrix}\code{\link[utils]{stack}} and \code{\link[reshape]{melt}} for other
functions producing long-format data.frames.}
\keyword{methods}
\arguments{\item{x}{a \code{hyperSpec} object}
\item{row.names,optional}{must be missing: they are not supported for hyperSpec objects.}
\item{\dots}{ignored}
\item{rownames}{should the rownames be in column \code{.rownames} of the
long-format data.frame?}
\item{wl.factor}{should the wavelengths be returned as a factor (instead of
numeric)?}
\item{na.rm}{if \code{TRUE}, rows where spc is not \code{NA} are deleted.}
}
\examples{
as.data.frame (chondro [1:3,, 600:620])
as.matrix (chondro [1:3,, 600:620])

as.wide.df (chondro [1:5,, 600 ~ 610])
summary (as.wide.df (chondro [1:5,, 600 ~ 610]))

as.long.df (flu [,, 405 ~ 410])
summary (as.long.df (flu [,, 405 ~ 410]))
summary (as.long.df (flu [,, 405 ~ 410], rownames = TRUE))
summary (as.long.df (flu [,, 405 ~ 410], wl.factor = TRUE))

df <- as.t.df (apply (chondro, 2, mean_pm_sd))
head (df)

if (require (ggplot2)){
ggplot (df, aes (x = .wavelength)) +
geom_ribbon (aes (ymin = mean.minus.sd, ymax = mean.plus.sd),
fill = "#00000040") +
geom_line (aes (y = mean))
}
}
\alias{as.matrix,hyperSpec-method}
\alias{as.matrix}
\alias{as.matrix}
\alias{as.matrix-methods}
\alias{as.matrix,ANY-method}
\alias{as.matrix,hyperSpec-method}
\alias{as.wide.df}
\alias{as.wide.df}
\alias{as.long.df}
\details{\code{as.long.df}: The data.frame returned by \code{as.long.df} is guaranteed to have columns
\code{spc} and \code{.wavelength}. If \code{nwl (x) == 0} these columns
will be \code{NA}.

}
\alias{as.long.df}
\alias{as.t.df}
\alias{as.t.df}

