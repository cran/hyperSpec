\name{spc.NA.linapprox}
\alias{spc.NA.linapprox}
\title{Impute missing data points...}
\usage{spc.NA.linapprox(spc, neighbours=1, ..., short="spc.NA.linapprox", user=NULL, date=NULL)
}
\description{Impute missing data points}
\details{Replace \code{NA}s in the spectra matrix by linear interpolation.}
\value{hyperSpec object}
\author{Claudia Beleites}
\arguments{\item{spc}{hyperSpec object with spectra matrix containing \code{NA}s}
\item{neighbours}{how many neighbour data points should be used to fit the line}
\item{...}{ignored}
\item{short,user,date}{handed to \code{\link{logentry}}}
}

