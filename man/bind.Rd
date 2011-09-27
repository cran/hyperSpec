\name{bind}
\alias{bind}
\title{Binding hyperSpec Objects...}
\usage{bind(direction=stop("direction ('c' or 'r') required"), ..., short="bind",
    user=NULL, date=NULL)

cbind.hyperSpec(..., short="cbind", deparse.level)


rbind.hyperSpec(..., short="rbind", deparse.level)


\S4method{cbind2}{hyperSpec,hyperSpec}(x, y)


\S4method{cbind2}{hyperSpec,missing}(x, y)


\S4method{rbind2}{hyperSpec,hyperSpec}(x, y)


\S4method{rbind2}{hyperSpec,missing}(x, y)

}
\description{Binding hyperSpec Objects
Two S3 functions \code{cbind.hyperSpec} and \code{rbind.hyperSpec} act as
an interfaces to \code{cbind2} and \code{rbind2} because neither
\code{\link[Matrix]{rBind}} and \code{\link[Matrix]{cBind}} nor S4 versions
of \code{cbind} and \code{rbind} do work at the moment.}
\details{\code{bind}: While it is now possible to do S4 despatch on \code{\dots{}}, defining such
S4 methods for \code{cbind} and \code{rbind} breaks the binding of
\code{Matrix} objects. Therefore, two S3 methods \code{rbind.hyperSpec} and
\code{cbind.hyperSpec} are defined.

\code{bind} does the common work for both column- and row-wise binding.

}
\alias{bind}
\value{\code{bind}: a \code{hyperSpec} object, possibly with different row order (for
\code{bind ("c", \dots{})} and \code{cbind2}).

}
\note{You might have to make sure that the objects either all have or all
do not have rownames and/or colnames.}
\author{C. Beleites}
\seealso{\code{\link[Matrix]{rBind}}, \code{\link[Matrix]{cBind}}
\code{\link[methods]{rbind2}}, \code{\link[methods]{cbind2}}
\code{\link[base]{rbind}}, \code{\link[base]{cbind}}

\code{\link{merge}} and \code{\link{collapse}} for combining objects that do not share spectra
or wavelengths, respectively.}
\keyword{methods}
\keyword{manip}
\arguments{\item{\dots}{The \code{hyperSpec} objects to be combined.

Alternatively, \emph{one} list of \code{hyperSpec} objects can be given to
\code{bind}.}
\item{deparse.level}{ignored.}
\item{short,user,date}{for the log}
\item{direction}{"r" or "c" to bind rows or columns}
\item{x,y}{\code{hyperSpec} objects}
}
\examples{
chondro
bind ("r", chondro, chondro)
rbind (chondro, chondro)
cbind (chondro, chondro)
bind ("r", list (chondro, chondro, chondro))

x <- chondro[,, 600 : 605]
x$a <- 1
x@data <- x@data[, sample (ncol (x), ncol (x))] # reorder columns

y <- chondro [nrow (chondro) : 1,, 1730 : 1750] # reorder rows
y$b <- 2

cbind2 (x, y) # works

y$y[3] <- 5
try (cbind2 (x, y)) # error
}
\alias{cbind.hyperSpec}
\alias{bind}
\alias{cbind.hyperSpec}
\alias{rbind.hyperSpec}
\alias{cbind2,hyperSpec,hyperSpec-method}
\alias{rbind2,hyperSpec,hyperSpec-method}
\alias{cbind2,hyperSpec,missing-method}
\alias{rbind2,hyperSpec,missing-method}
\alias{cbind.hyperSpec}
\alias{rbind.hyperSpec}
\alias{rbind.hyperSpec}
\alias{rbind.hyperSpec}
\alias{cbind2,hyperSpec,hyperSpec-method}
\alias{cbind2}
\alias{cbind2,hyperSpec,hyperSpec-method}
\alias{cbind2,hyperSpec,missing-method}
\alias{cbind2}
\alias{cbind2,hyperSpec,missing-method}
\alias{rbind2,hyperSpec,hyperSpec-method}
\alias{rbind2}
\alias{rbind2,hyperSpec,hyperSpec-method}
\alias{rbind2,hyperSpec,missing-method}
\alias{rbind2}
\alias{rbind2,hyperSpec,missing-method}

