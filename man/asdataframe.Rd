% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.data.frame.R
\docType{methods}
\name{as.data.frame}
\alias{as.data.frame}
\alias{as.data.frame.hyperSpec}
\alias{as.data.frame,hyperSpec-method}
\alias{as.matrix.hyperSpec}
\alias{as.matrix}
\alias{as.matrix,hyperSpec-method}
\alias{as.wide.df}
\alias{as.long.df}
\alias{as.t.df}
\title{Conversion of a hyperSpec object into a data.frame or matrix
\code{as.data.frame} returns \code{x@data} (as data.frame) \code{as.matrix}
returns the spectra matrix \code{x@data$spc} as matrix}
\usage{
\method{as.data.frame}{hyperSpec}(x, row.names = TRUE, optional = NULL, ...)

\method{as.matrix}{hyperSpec}(x, ...)

as.wide.df(x, wl.prefix = "")

as.long.df(x, rownames = FALSE, wl.factor = FALSE, na.rm = TRUE)

as.t.df(x)
}
\arguments{
\item{x}{a \code{hyperSpec} object}

\item{row.names}{if \code{TRUE}, a column \code{.row} is created containing row names or row
indices if no rownames are set. If character vector, the rownames are set accordingly.}

\item{optional}{ignored}

\item{...}{ignored}

\item{wl.prefix}{prefix to prepend wavelength column names}

\item{rownames}{should the rownames be in column \code{.rownames} of the
long-format data.frame?}

\item{wl.factor}{should the wavelengths be returned as a factor (instead of
numeric)?}

\item{na.rm}{if \code{TRUE}, rows where spc is not \code{NA} are deleted.}
}
\value{
\code{x@data} and \code{x@data$spc} (== \code{x$spc} == \code{x [[]]}), respectively.

\code{as.wide.df} returns a data.frame that consists of the extra data and
the spectra matrix converted to a data.frame. The spectra matrix is
expanded \emph{in place}.

\code{as.long.df} returns the stacked or molten version of \code{x@data}. The
  wavelengths are in column \code{.wavelength}.

\code{as.t.df} returns a data.frame similar to \code{as.long.df}, but each
  spectrum in its own column. This is useful for exporting summary spectra,
  see the example.
}
\description{
The data.frame returned by \code{as.long.df} is guaranteed to have columns
\code{spc} and \code{.wavelength}. If \code{nwl (x) == 0} these columns
will be \code{NA}.
}
\examples{

as.data.frame (chondro [1:3,, 600 ~ 620])
as.matrix (chondro [1:3,, 600 ~ 620])
lm (c ~ spc, data = flu [,,450])

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
\seealso{
\code{\link[base]{as.data.frame}}

and \code{\link[base:matrix]{base::as.matrix()}}

[\code{[[}()] (\verb{[[]]}) for a shortcut to \code{as.matrix}

\code{\link[utils]{stack}} and \code{\link[reshape]{melt}} or \code{reshape2::melt()} for
other functions producing long-format data.frames.
}
\author{
C. Beleites
}
\keyword{methods}
