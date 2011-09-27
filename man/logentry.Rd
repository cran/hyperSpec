\name{logentry}
\alias{logentry}
\title{Append a Row to the logbook of a hyperSpec Object...}
\usage{logentry(x, short=NULL, long=NULL, date=NULL, user=NULL)
}
\description{Append a Row to the logbook of a hyperSpec Object
A log entry is generated and appended to the log of \code{x}.}
\details{The arguments (besides x) go into the respective columns of \code{x@log}.

The following values are used for any arguments that are \code{NULL}:

The name of the funtion that called \code{logentry} is used for
\code{short}, and its call for \code{long}.

For \code{date}, the current \code{\link[base]{Sys.time}()} is used, and
\code{user} is constructed from \code{\link[base]{Sys.info}()}'s
\code{user} and \code{nodename}.}
\value{\code{x@log} (\emph{\code{data.frame}}) including the new row.}
\author{C. Beleites}
\arguments{\item{x}{a \code{hyperSpec} object}
\item{short}{short description}
\item{long}{long description, e.g. list with function arguments}
\item{date}{time stamp}
\item{user}{username}
}
\examples{
logentry (chondro, short = "test")
}

