\name{options}
\alias{hy.getOptions}
\title{Options for package hyperSpec...}
\usage{hy.getOptions(...)

hy.getOption(name)


hy.setOptions(...)

}
\description{Options for package hyperSpec
Functions to access and set hyperSpec's options.}
\details{\code{hy.getOptions}: Currently, the following options are defined: \tabular{llll}{ \bold{Name}
\tab \bold{Default Value (range)} \tab \bold{Description} \tab \bold{Used
by}\cr debuglevel \tab 0 (1L 2L) \tab amount of debugging information
produced \tab \code{\link{spc.identify}} \code{\link{map.identify}}\cr gc
\tab FALSE \tab triggers frequent calling of gc () \tab
\code{\link{read.ENVI}}, \code{new ("hyperSpec")}\cr log \tab TRUE \tab
automatically create entries \tab \code{\link{logbook}}\cr }

\code{hy.setOptions} will discard any values that were given without a
name.

}
\value{\code{hy.getOptions}: \tabular{ll}{
\code{hy.getOptions} \tab returns a list of all options\cr
\code{hy.setOptions} \tab invisibly returns a list with the options \cr
\code{hy.getOption}  \tab returns the value of the requested option \cr
}

}
\author{C. Beleites}
\keyword{misc}
\arguments{\item{\dots}{\code{hy.setOptions}: pairs of argument names and values.

\code{hy.getOptions}: indices (or names) of the options.}
\item{name}{the name of the option}
}
\examples{
hy.getOptions ()
}
\alias{hy.getOption}
\alias{hy.setOptions}

