\name{apply-methods}
\docType{methods}
\alias{apply,hyperSpec-method}
\title{apply}
\description{
 Computes summary statistics for the spectra of a \code{hyperSpec} object. 
}
\usage{
\S4method{apply}{hyperSpec}(X, MARGIN, FUN, \dots,
 	label.wl = NULL, label.spc = NULL, new.wavelength = NULL,
   short = NULL, long = NULL, user = NULL, date = NULL)
}
\arguments{
  \item{X, spc}{a \code{hyperSpec} object}
  \item{MARGIN}{The subscript which the function will be
    applied over. \code{1} indicates rows, \code{2} indicates columns,
	 \code{1:2} indicates that \code{FUN} should be applied to all
	 elements of the spectra matrix.
    }
  \item{FUN}{function to compute the summary statistics}
  \item{\dots}{further arguments passed to \code{FUN}}
  \item{label.wl,label.spc}{new labels for wavelength and spectral intensity axes}
  \item{new.wavelength}{for \code{MARGIN = 2}: numeric vector or name of the argument in \dots 
  that is to be used (character) as wavelength axis of the resulting object.}
  \item{short, long, user, date}{aguments passed to \code{logentry}}
}
\value{
  A \code{hyperSpec} object
}

\details{
   \code{apply} gives the functionality of
	\code{\link[base]{apply}} for \code{hyperSpec} objects.

  The generic functions of group \code{\link[methods]{Math}} are not
  definded for \code{hyperSpec} objects. Instead, \code{apply} can be
  used. For functions like \code{log} that work on scalars,
  \code{MARGIN = 1 : 2} gives the appropriate behaviour.
  
  \code{spcapply} does the same as \code{apply} with  \code{MARGIN = 1}, 
  but additionally allows to set a new wavelength axis and adjust the labels.     
    
  \code{wlapply} does the same as \code{apply} with  \code{MARGIN = 2}, 
  but additionally allows to set a new wavelength axis and adjust the labels.     
}
\seealso{
  \code{\link[base]{apply}}, for applying \code{FUN} to subgroups of
  the \code{hyperSpec} object:  \code{\link{aggregate}}.
}
\examples{

plot (apply (chondro, 2, range), "spc")

plot (apply (chondro, 1, mean), "map")

flu[[,,405:407]]
apply (flu, 1:2, "*", -1)[[,,405:407]]

all (apply (flu, 1:2, log)[[]] - log(flu[[]]) == 0)
}

\author{C. Beleites}

\keyword{methods}
\keyword{iteration}

