##'  functions for hyperSpec objects
##'
##' hyperSpec objects can use the base functions \code{\link[base]{colMeans}},
##' \code{\link[base]{colSums}}, \code{\link[base]{rowMeans}} and \code{\link[base]{rowSums}}.
##'
##' @param x hyperSpec object
##' @param label.spc labels for the intensity axis for loadings-like (col) statistics
##' @param label.wavelength labels for the wavelength axis for scores-like (row) statistics
##' @param user,short,date handed to \code{\link[hyperSpec]{logentry}}
##' @param na.rm,... further parameters to the base functions
##' @seealso \link[base]{colSums}
##' @rdname colSums
##' @name colSums
NULL
 
##' @noRd
setGeneric ('colMeans')#, package = 'matrixStats')

##' @rdname colSums
##' @export
 setMethod ("colMeans", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.spc, 
         user = NULL, short = "colMeans", date = NULL){
   result <- colMeans (x@data$spc, na.rm = na.rm, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMeans) <- function (){
  checkEqualsNumeric (colMeans (chondro)[[]], colMeans (chondro [[]]))
}

##' @noRd
setGeneric ('colSums') #, package = 'matrixStats')

##' @rdname colSums
##' @export
 setMethod ("colSums", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.spc, 
         user = NULL, short = "colSums", date = NULL){
   result <- colSums (x@data$spc, na.rm = na.rm, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colSums) <- function (){
  checkEqualsNumeric (colSums (chondro)[[]], colSums (chondro [[]]))
}

##' @noRd
setGeneric ('rowMeans') #, package = 'matrixStats')

##' @rdname colSums
##' @export
 setMethod ("rowMeans", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.wavelength,
          user = NULL, short = "rowMeans", date = NULL){
   result <- rowMeans (x@data$spc, na.rm = na.rm, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMeans) <- function (){
  checkEqualsNumeric (rowMeans (chondro)[[]], rowMeans (chondro [[]]))
}

##' @noRd
setGeneric ('rowSums') #, package = 'matrixStats')

##' @rdname colSums
##' @export
 setMethod ("rowSums", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.wavelength,
          user = NULL, short = "rowSums", date = NULL){
   result <- rowSums (x@data$spc, na.rm = na.rm, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowSums) <- function (){
  checkEqualsNumeric (rowSums (chondro)[[]], rowSums (chondro [[]]))
}

