\name{wc}
\alias{wc}
\title{wc}
\usage{wc(file, flags=c("lines", "words", "bytes"))}
\description{word count of ASCII files}
\details{wc uses the system command wc}
\value{data.frame with the counts and file names, or \code{NULL} if wc is not available}
\author{C. Beleites}
\arguments{\item{file}{the file name or pattern}
\item{flags}{the parameters to count, character vector with the long form of the parameters}}
