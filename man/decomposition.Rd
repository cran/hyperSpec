\name{decomposition}
\alias{decomposition}
\title{Convert Principal Component Decomposition or the like into a
  hyperSpec Object}
\description{
  Decomposition of the spectra matrix is a common procedure in
  chemometrix data analysis. \code{decomposition} converts the result
  matrices into new 
\code{hyperSpec} objects.}
\usage{
decomposition(object, x, wavelength = seq_len (ncol (x)),
              label.wavelength, label.spc, scores = TRUE,
              retain.columns = FALSE,short = "", \dots)
}
\arguments{
  \item{object}{A \code{hyperSpec} object.}
  \item{x}{matrix with the new content for \code{object@data$spc}.

    May correspond to rows (like a scores matrix) or columns (like a
    loadings matrix) of \code{object}.}
  
  \item{wavelength}{for a scores-like \code{x}: the new
    \code{object@wavelength}.}

  \item{label.wavelength}{The new label for the wavelength axis (if
    \code{x} is scores-like)}

  \item{label.spc}{The new label for the spectra matrix}

  \item{scores}{is \code{x} a scores-like matrix?}


  \item{retain.columns}{for loading-like decompostition (i.e. \code{x}
    holds loadings, pure component spectra or the like), the data columns
    need special attention.

    Columns with different values across the rows will be set to
    \code{NA} if \code{retain.columns} is \code{TRUE}, otherwise they
    will be deleted.
  }

  \item{short, \dots}{handed over to \code{logentry}}
}
\details{
  Multivariate data are frequently decomposed by methods like principal
  component analysis, partial least squares, linear discriminant
  analysis, and the like.
  These methods yield loadings (or latent
  variables, components, \dots) that are linear combination coefficients
  along the wavelength axis and scores for each spectrum and loading.

  The loadings matrix gives a coordinate transformation, and the scores are
  values in that new coordinate system.

  The obtained loadings are spectra-like objects: a loading has a
  coefficient for each wavelength. If such a matrix (with the same
  number of columns as \code{object} has wavelengths) is given to
  \code{decomposition}, the spectra matrix is replaced by
  \code{x}. Moreover,  all columns of \code{object@data} that did not
  contain the same value for all spectra are set to \code{NA}.
  Thus, for the resulting \code{hyperSpec} object, \code{\link{plotspc}}
  and related functions are meaningful. \code{\link[hyperSpec]{plotmap}}
  cannot be applied as the loadings are not laterally resolved. 
  
  The scores-matrix needs to have the same number of rows as
  \code{object} has spectra. If such a matrix is given, the spectra
  matrix is replaced by \code{x} and \code{object@wavelength} is
  replaced by \code{wavelength}. The information related to each of the
  spectra is retained. For such a \code{hyperSpec} object,
  \code{\link{plotmap}} and \code{\link{plotc}} and the like can be
  applied. Of couse, it is also possible to use the spectra plotting,
  but the interpretation is not that of the spectrum any longer. 
}
\value{
  A \code{hyperSpec} object, updated according to \code{x}
}
\author{C. Beleites}
\seealso{
  See \code{\link{\%*\%}} for matrix multiplication of \code{hyperSpec}
  objects. 

  See e.g. \code{\link[stats]{prcomp}} and \code{\link[stats]{princomp}} for
  principal component analysis, and package \code{pls} for Partial Least
  Squares Regression. 
}
\examples{
  pca <- prcomp (~ spc, data = flu$., center = FALSE)

  scores <- decomposition (flu, pca$x, label.wavelength = "PC",
                           label.spc = "score / a.u.")

  loadings <- decomposition (flu, t(pca$rotation), scores = FALSE,
     label.spc = "loading I / a.u.")

  plotspc (loadings, stacked = TRUE, col = matlab.palette(6))

  plotc (scores[,,1], plot.args = list(ylim = range(scores[[]])))
  for (i in 2 : nwl (scores))
     plotc (scores[,,i], add = TRUE, plot.args = list (col = matlab.palette(6)[i]))
  pca$sdev

  ## everything besides the first component is just noise
  ## Reconstructing the data using only the first PC results in a noise
  ## filtered data set.

  flu.filtered <- scores[,,1] %*% loadings[1,,]


  ## example 2
  pca <- prcomp (~ spc, data = chondro$., tol = 0.1)

  scores <- decomposition (chondro, pca$x, label.wavelength = "PC",
                           label.spc = "score / a.u.")

  plotmap (scores[,,1])
} 
\keyword{methods}
\keyword{manip}

