\name{spc.fit.poly}
\alias{spc.fit.poly}
\alias{spc.fit.poly.below}
\title{Polynomial Baseline Fitting}
\description{
  These functions fit polynomal baselines. 
}
\usage{
spc.fit.poly(fit.to, apply.to = NULL, poly.order = 1, 
             short = NULL, user = NULL, date = NULL)
spc.fit.poly.below  (fit.to, apply.to = fit.to, poly.order = 1, npts.min = NULL,
                     noise = 0, 
                     short = NULL, user = NULL, date = NULL)
}
\arguments{
  \item{fit.to}{\code{hyperSpec} object on which the baselines are fitted}
  \item{apply.to}{\code{hyperSpec} object on which the baselines are evaluted
  If \code{NULL}, a \code{hyperSpec} object containing the polynomial coefficients 
  rather than evaluted baselines is returned.} 
  \item{poly.order}{order of the polynomial to be used}
  \item{npts.min}{minmal number of points used for fitting the polynomial}
  \item{noise}{noise level to be considered during the fit. It may be given as 
  one value for all the spectra, or for each spectrum separately.}
  \item{short, user, date}{handed to \code{logentry}}
}
\details{
  Both functions fit polynomials to be used as baselines. If \code{apply.to} is \code{NULL}, 
  a \code{hyperSpec} object with the polynomial coefficients is returned, 
  otherwise the polynomials are evaluated on the spectral range of \code{apply.to}.
  
  \code{spc.fit.poly} calculates the least squares fit of order \code{poly.order} 
  to the \emph{complete} spectra given in \code{fit.to}. Thus \code{fit.to} needs to be cut appropriately.
  
  \code{spc.fit.poly.below} tries to fit the baseline on appropriate spectral ranges of the spectra in \code{fit.to}. 
  For details, see \url{../doc/baselinebelow.pdf}.
}
\value{
  \code{hyperspec} object containing the baselines in the spectra matrix,
  either as polynomial coefficients or as polynomials evaluted on the spectral range of \code{apply.to} 
}
\seealso{\url{../doc/baselinebelow.pdf}}
\author{C. Beleites}
\examples{
baselines <- spc.fit.poly(chondro[,, c (625 ~ 640, 1785 ~ 1800)], chondro)
plot(chondro - baselines, "spcprctl5")

baselines <- spc.fit.poly.below(chondro)
plot(chondro - baselines, "spcprctl5")

vignette ("baselinebelow", package = "hyperSpec")
}
\keyword{manip}
\keyword{datagen}
