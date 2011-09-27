\name{split}
\alias{split,hyperSpec-method}
\alias{split}
\title{Split a hyperSpec object according to groups...}
\usage{\S4method{split}{hyperSpec}(x, f, drop=TRUE, short=NULL, user=NULL, date=NULL)
}
\description{Split a hyperSpec object according to groups
\code{split} divides the \code{hyperSpec} object into a list of
\code{hyperSpec} objects according to the groups given by \code{f}.}
\details{The \code{hyperSpec} objects in the list may be bound together again by
\code{\link{bind} ("r", list_of_hyperSpec_objects)}.}
\alias{split}
\alias{split-methods}
\alias{split,ANY-method}
\alias{split,hyperSpec-method}
\docType{methods}
\value{A list of \code{hyperSpec} objects.}
\author{C. Beleites}
\seealso{\code{\link[base]{split}}}
\keyword{methods}
\arguments{\item{x}{the \code{hyperSpec} object}
\item{f}{a factor giving the grouping (or a variable that can be converted
into a factor by \code{as.factor})}
\item{drop}{if \code{TRUE}, levels of\code{f} that do not occur are
dropped.}
\item{short,user,date}{handed to \code{\link{logentry}}}
}
\examples{
dist <- pearson.dist (chondro[[]])
dend <- hclust (dist, method = "ward")
z <- cutree (dend, h = 0.15)

clusters <- split (chondro, z)
length (clusters)

# difference in cluster mean spectra
plot (apply (clusters[[2]], 2, mean) - apply (clusters[[1]], 2, mean))
}

