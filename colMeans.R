##' matrixStats functions for hyperSpec objects
##'
##' hyperSpec objects can use matrix functions from package \link[matrixStats]{matrixStats-package}
##' in addition to the base functions \code{\link[base]{colMeans}}, \code{\link[base]{colSums}},
##' \code{\link[base]{rowMeans}} and \code{\link[base]{rowSums}}.
##'
##' @param x hyperSpec object
##' @param label.spc labels for the intensity axis for loadings-like statistics
##' @param label.wavelength labels for the wavelength axis for scores-like statistics
##' @param user,short,date handed to \code{\link[hyperSpec]{logentry}}
##' @param drop,na.rm,... further parameters to the \link[matrixStats]{matrixStats-package} function
##' @rdname matrixStats
##' @name matrixStats
NULL
 
##' @noRd
setGeneric ('colMeans', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMeans", date = NULL){
   result <- colMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMeans) <- function (){
   colMeans (chondro)
}

##' @noRd
setGeneric ('colSums', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colSums", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSums", date = NULL){
   result <- colSums (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colSums) <- function (){
   colSums (chondro)
}

##' @noRd
setGeneric ('rowMeans', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMeans", date = NULL){
   result <- rowMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMeans) <- function (){
   rowMeans (chondro)
}

##' @noRd
setGeneric ('rowSums', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowSums", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSums", date = NULL){
   result <- rowSums (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowSums) <- function (){
   all.equal (rowSums (chondro)[[]], rowSums (chondro[[]]))
}

