\name{labels}
\Rdversion{1.1}
\alias{labels,hyperSpec-method}
\alias{labels<-}
\title{Get and Set Labels of a hyperSpec Object}
\description{
\code{labels}
}
\usage{
\S4method{labels}{hyperSpec}(object, which = NULL, drop = TRUE, \dots)
labels (object, which = NULL, ...) <- value
}
\arguments{
  \item{object}{a hyperSpec object}
  \item{which}{numeric or character to specify the label(s)}
  \item{drop}{if the result would be a list with only one element, 
  	should the element be returned instead?}
  \item{\dots}{handed to \code{\link[hyperSpec]{logentry}}}
  \item{value}{the new label(s)}
}
\details{
\code{value} may be a list or vector of labels giving the new label for each of the 
entries specified by \code{which}.  

The names of the labels are the same as the colnames of the \code{data.frame}. 
The label for the wavelength axis has the name \code{.wavelength}.

The labels should be
    given in a form ready for the text-drawing functions (see
    \code{\link[grDevices]{plotmath}}), e.g. as \code{expression} or a \code{character}.
}
\value{
\code{labels} returns a list of labels. 
If \code{drop} is \code{TRUE} and the list contains only one element, the element is returned instead.

\code{labels<-} returns a \code{hyperSpec} object.
}
\author{C. Beleites}
\seealso{
\code{\link[base]{labels}}
}
\examples{
labels (chondro)
labels (flu, "c") <- expression ("/" ("c", "mg / l"))
}

