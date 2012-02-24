\name{spc.fit.poly}
\alias{spc.fit.poly}
\alias{spc.fit.poly.below}
\title{Polynomial Baseline Fitting
These functions fit polynomal baselines.}
\usage{
  spc.fit.poly(fit.to, apply.to = NULL, poly.order = 1,
    short = "spc.fit.poly", user = NULL, date = NULL)

  spc.fit.poly.below(fit.to, apply.to = fit.to,
    poly.order = 1, npts.min = NULL, noise = 0,
    short = "spc.fit.poly.below", user = NULL, date = NULL)
}
\arguments{
  \item{fit.to}{\code{hyperSpec} object on which the
  baselines are fitted}

  \item{apply.to}{\code{hyperSpec} object on which the
  baselines are evaluted If \code{NULL}, a \code{hyperSpec}
  object containing the polynomial coefficients rather than
  evaluted baselines is returned.}

  \item{poly.order}{order of the polynomial to be used}

  \item{short,user,date}{handed to \code{logentry}}

  \item{npts.min}{minmal number of points used for fitting
  the polynomial}

  \item{noise}{noise level to be considered during the fit.
  It may be given as one value for all the spectra, or for
  each spectrum separately.}
}
\value{
  \code{hyperspec} object containing the baselines in the
  spectra matrix, either as polynomial coefficients or as
  polynomials evaluted on the spectral range of
  \code{apply.to}
}
\description{
  Both functions fit polynomials to be used as baselines.
  If \code{apply.to} is \code{NULL}, a \code{hyperSpec}
  object with the polynomial coefficients is returned,
  otherwise the polynomials are evaluated on the spectral
  range of \code{apply.to}.

  \code{spc.fit.poly.below} tries to fit the baseline on
  appropriate spectral ranges of the spectra in
  \code{fit.to}.  For details, see the \code{vignette
  ("baseline")}.
}
\details{
  \code{spc.fit.poly} calculates the least squares fit of
  order \code{poly.order} to the \emph{complete} spectra
  given in \code{fit.to}. Thus \code{fit.to} needs to be
  cut appropriately.
}
\examples{
\dontrun{vignette ("baseline", package = "hyperSpec")}

baselines <- spc.fit.poly(chondro[,, c (625 ~ 640, 1785 ~ 1800)], chondro)
plot(chondro - baselines, "spcprctl5")
baselines <- spc.fit.poly.below(chondro)
plot(chondro - baselines, "spcprctl5")
}
\author{
  C. Beleites
}
\seealso{
  \code{vignette ("baseline", package = "hyperSpec")}
}
\concept{
  baseline
}
\keyword{datagen}
\keyword{manip}

