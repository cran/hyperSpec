##' Convert Principal Component Decomposition or the like into a hyperSpec Object
##' Decomposition of the spectra matrix is a common procedure in chemometrix data
##' analysis. \code{decomposition} converts the result matrices into new \code{hyperSpec} objects.
##' 
##' Multivariate data are frequently decomposed by methods like principal component analysis, partial
##' least squares, linear discriminant analysis, and the like.  These methods yield loadings (or
##' latent variables, components, \dots{}) that are linear combination coefficients along the
##' wavelength axis and scores for each spectrum and loading.
##' 
##' The loadings matrix gives a coordinate transformation, and the scores are values in that new
##' coordinate system.
##' 
##' The obtained loadings are spectra-like objects: a loading has a coefficient for each
##' wavelength. If such a matrix (with the same number of columns as \code{object} has wavelengths)
##' is given to \code{decomposition}, the spectra matrix is replaced by \code{x}. Moreover, all
##' columns of \code{object@@data} that did not contain the same value for all spectra are set to
##' \code{NA}.  Thus, for the resulting \code{hyperSpec} object, \code{\link{plotspc}} and related
##' functions are meaningful.  \code{\link[hyperSpec]{plotmap}} cannot be applied as the loadings are
##' not laterally resolved.
##' 
##' The scores-matrix needs to have the same number of rows as \code{object} has spectra. If such a
##' matrix is given, the spectra matrix is replaced by \code{x} and \code{object@@wavelength} is
##' replaced by \code{wavelength}. The information related to each of the spectra is retained. For
##' such a \code{hyperSpec} object, \code{\link{plotmap}} and \code{\link{plotc}} and the like can be
##' applied. Of couse, it is also possible to use the spectra plotting, but the interpretation is not
##' that of the spectrum any longer.
##' 
##' @param object A \code{hyperSpec} object.
##' @param x matrix with the new content for \code{object@@data$spc}.
##' 
##' May correspond to rows (like a scores matrix) or columns (like a loadings matrix) of
##' \code{object}.
##' @param wavelength for a scores-like \code{x}: the new \code{object@@wavelength}.
##' @param label.wavelength The new label for the wavelength axis (if \code{x} is scores-like)
##' @param label.spc The new label for the spectra matrix
##' @param scores is \code{x} a scores-like matrix?
##' @param retain.columns for loading-like decompostition (i.e. \code{x} holds loadings, pure
##' component spectra or the like), the data columns need special attention.
##' 
##' Columns with different values across the rows will be set to \code{NA} if \code{retain.columns}
##' is \code{TRUE}, otherwise they will be deleted.
##' @param short,user,date handed over to \code{logentry}
##' @param \dots ignored.
##' @return A \code{hyperSpec} object, updated according to \code{x}
##' @author C. Beleites
##' @seealso See \code{\link{\%*\%}} for matrix multiplication of \code{hyperSpec} objects.
##' 
##' See e.g. \code{\link[stats]{prcomp}} and \code{\link[stats]{princomp}} for principal component
##' analysis, and package \code{pls} for Partial Least Squares Regression.
##' @keywords methods manip
##' @include apply.R 
##' @export
##' @examples
##' 
##'   pca <- prcomp (~ spc, data = flu$., center = FALSE)
##' 
##'   scores <- decomposition (flu, pca$x, label.wavelength = "PC",
##'                            label.spc = "score / a.u.")
##' 
##'   loadings <- decomposition (flu, t(pca$rotation), scores = FALSE,
##'      label.spc = "loading I / a.u.")
##' 
##'   plotspc (loadings, stacked = TRUE, col = matlab.palette(6))
##' 
##'   plotc (scores, groups = .wavelength,col = matlab.palette(6), type = "b")
##'   pca$sdev
##' 
##'   ## everything besides the first component is just noise
##'   ## Reconstructing the data using only the first PC results in a noise
##'   ## filtered data set.
##' 
##'   flu.filtered <- scores[,,1] %*% loadings[1,,]
##' 
##' 
##'   ## example 2
##'   pca <- prcomp (~ spc, data = chondro$., tol = 0.1)
##' 
##'   scores <- decomposition (chondro, pca$x, label.wavelength = "PC",
##'                            label.spc = "score / a.u.")
##' 
##'   plotmap (scores[,,1])
##' 
decomposition <- function (object, x, wavelength = seq_len (ncol (x)),
                           label.wavelength, label.spc,
                           scores = TRUE, retain.columns = FALSE,
                           short = "decomposition", user = NULL, date = NULL,
                           ...){
  validObject (object)

  if (is.vector (x))
    if (nrow (object) == length (x))
      dim (x) <- c(length (x), 1)
    else
      dim (x) <- c(1, length (x))

  if ((nrow (x) == nrow (object)) && scores){

    object@data$spc <- I (as.matrix (x))

    .wl (object) <- wavelength

    if (!missing (label.wavelength)) object@label$.wavelength <- label.wavelength

  } else if (ncol (x) == nwl (object)){
    spc <- match("spc", colnames(object@data))
    
    ## apply changes type of retained columns to character!!!
    ## must be done in a loop one column after the other or a matrix in a column
    ## (e.g. for the independent variate of PLS) will cause an error 

    cols <- rep (TRUE, ncol (object@data))

    for (i in seq_len (ncol (object@data)) [-spc]) {
      tmp <- as.data.frame (lapply (object@data[, i, drop = FALSE], .na.if.different))
      object@data [1, i] <- tmp
      if (all (is.na (tmp)))
        cols [i] <- FALSE
    }
    if (!retain.columns) 
      object@data <- object@data[, cols, drop = FALSE]

    object@data <- object@data[rep(1, nrow(x)), , drop = FALSE]
    object@data$spc <- I(as.matrix(x))

  } else {
    stop ("Either rows (if scores == TRUE) or columns (if scores == FALSE) of",
          " x and object must correspond")
  }

  rownames (object@data) <- rownames (x)
  
  if (!missing (label.spc)) object@label$spc <- label.spc
  
  validObject (object)

  .logentry (object, short = short,  user = user, date = date)
}

.test (decomposition) <- function (){
  rm (flu)
    ## POSIXct
  flu$ct <- as.POSIXct(Sys.time()) 
  checkEquals (decomposition (flu, flu [[]], scores = FALSE)$ct, flu$ct)

  ## POSIXlt
  flu$lt <- as.POSIXlt(Sys.time()) 
  checkEquals (decomposition (flu, flu [[]], scores = FALSE)$lt, flu$lt)

  rm (flu)
}
