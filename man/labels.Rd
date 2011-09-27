\name{labels}
\alias{labels<-}
\title{labels<-}
\usage{labels (object, which = NULL, ..., short = "labels<-", user = NULL, date = NULL) <- value
\S4method{labels}{hyperSpec}(object, which = bquote (), drop = TRUE, \dots,
use.colnames = TRUE)
}
\value{\code{labels<-}: \code{labels<-} returns a \code{hyperSpec} object.

\code{labels,hyperSpec-method}: \code{labels} returns a list of labels.  If \code{drop} is
\code{TRUE} and the list contains only one element, the element is
returned instead.


}
\arguments{\item{value}{the new label(s)}
\item{short,user,date}{handed to \code{\link[hyperSpec]{logentry}}}
\item{object}{a hyperSpec object}
\item{which}{numeric or character to specify the label(s)}
\item{\dots}{ignored}
\item{drop}{if the result would be a list with only one element, should the
element be returned instead?}
\item{use.colnames}{should missing labels be replaced by column names of
the extra data?}
}
\examples{
labels (flu, "c") <- expression ("/" ("c", "mg / l"))

labels (chondro)
}
\alias{labels,hyperSpec-method}
\alias{labels}
\description{Get and Set Labels of a hyperSpec Object
\code{value} may be a list or vector of labels giving the new label for
each of the entries specified by \code{which}.}
\details{\code{labels,hyperSpec-method}: The names of the labels are the same as the colnames of the
\code{data.frame}.  The label for the wavelength axis has the name
\code{.wavelength}.

The labels should be given in a form ready for the text-drawing functions
(see \code{\link[grDevices]{plotmath}}), e.g. as \code{expression} or a
\code{character}.

}
\docType{methods}
\author{C. Beleites}
\seealso{\code{\link[base]{labels}}}

