
### use manually generated .Rd as aliases are not properly escaped.

##' Arithmetical Operators: +, -, *, /, ^, \%\%, \%/\%, \%*\% for hyperSpec objects
##'
##' The arithmetical operators \code{+}, \code{-}, \code{*}, \code{/}, \code{\^}, \code{\%\%},
##' \code{\%/\%},  and \code{\%*\%} for \code{hyperSpec} objects. 
##' 
##' You can use these operators in different ways:
##' \preformatted{
##' e1 + e2
##' `+` (e1, e2)
##' 
##' x \%*\% y
##' `\%*\%`(x, y)
##' 
##' -x }
##' The arithmetical operators \code{+}, \code{-}, \code{*}, \code{/}, \code{^}, \code{\%\%},
##' \code{\%/\%}, and \code{\%*\%} work on the  spectra matrix of the \code{hyperSpec} object. They
##' have their usual meaning (see \code{\link[base]{Arithmetic}}).  The operators work also with one
##' \code{hyperSpec} object and a numeric object or a matrices of the same size as the spectra matrix
##' of the \code{hyperSpec} object.
##' 
##' With numeric vectors \code{\link[hyperSpec]{sweep}} is most probably more appropriate.
##'   
##' If you want to calculate on the extra data as well, use the data.frame \code{hyperSpec@@data}
##' directly or \code{\link[hyperSpec]{as.data.frame} (x)}.
##' @author C. Beleites
##' 
##' @noRd
##' @param e1,e2 or
##' @param x,y either two \code{hyperSpec} objects or one \code{hyperSpec} object and  matrix of same
##' size as \code{hyperSpec[[]]} or a scalar (numeric of length 1).
##' @return \code{hyperSpec} object with the new spectra matrix.
##' @export
##' @aliases Arith-method Arith,hyperSpec-method Arith,hyperSpec,hyperSpec-method
##' +,hyperSpec,hyperSpec-method -,hyperSpec,hyperSpec-method *,hyperSpec,hyperSpec-method
##' ^,hyperSpec,hyperSpec-method /,hyperSpec,hyperSpec-method Arith,hyperSpec,matrix-method
##' Arith,hyperSpec,numeric-method Arith,hyperSpec,missing-method Arith,matrix,hyperSpec-method
##' Arith,numeric,hyperSpec-method
##' @keywords methods
##' @include paste.row.R
##' @docType methods
##' @concept hyperSpec arithmetic
##' @concept hyperSpec arithmetical operators
##' @concept hyperSpec plus
##' @concept hyperSpec division
##' @concept hyperSpec spectra conversion
##' @usage
##' x %*% y
##' x %% y
##' x %/% y
##' @seealso
##' \code{\link[hyperSpec]{sweep-methods}} for calculations involving a vector and
##'   the spectral matrix.
##'   
##'   \code{\link[methods]{S4groupGeneric}} for group generic methods.
##' 
##'   \code{\link[base]{Arithmetic}} for the base arithmetic functions.
##' 
##'   \code{\link[hyperSpec]{Comparison}} for comparison operators, 
##'   \code{\link[hyperSpec]{Math}} for mathematical group generic 
##'   functions (Math and Math2 groups) working on \code{hyperSpec} objects.
##' @examples
##' chondro + chondro
##' 1 / chondro
##' all((chondro + chondro - 2 * chondro)[[]] == 0)
##' -flu

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)
             if (.Generic %in% c ("*", "^", "%%", "%/%", "/"))
               warning (paste ("Do you really want to use", .Generic, "on 2 hyperSpec objects?"))
             e1 [[]] <- callGeneric (e1[[]], e2[[]])
             .logentry (e1, short = .Generic, long = as.character (e2))
           }
           )

.arithx <- function (e1, e2){
  validObject (e1)
  if (missing (e2)){
    e1  [[]] <- callGeneric (e1 [[]])
    .logentry (e1, short = .Generic, long = list ())
  } else {
    e1  [[]] <- callGeneric (e1 [[]], e2)
    .logentry (e1, short = .Generic,
               long = list (e2 = .paste.row (e2, val = TRUE)))
  }
}
##' @noRd
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "numeric"), .arithx)
##' @noRd
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "matrix"), .arithx)
##' @noRd
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "missing"), .arithx)

.arithy <- function (e1, e2){
  validObject (e2)
  e2  [[]] <- callGeneric (e1, e2 [[]])
  .logentry (e2, short = .Generic, long = list (e1 = .paste.row (e1, val = TRUE)))
}
##' @noRd
setMethod ("Arith", signature (e1 = "numeric", e2 = "hyperSpec"), .arithy)
##' @noRd
setMethod ("Arith", signature (e1 = "matrix", e2 = "hyperSpec"), .arithy)

##' @noRd
##' @concept hyperSpec matrix multiplication
##' @aliases \%*\% \%*\%,hyperSpec,hyperSpec-method \%*\%,matrix,hyperSpec-method
##' \%*\%,hyperSpec,matrix-method
##' @export "%*%"
##' @seealso  \code{\link[base]{matmult}} for matrix multiplications with \code{\%*\%}.
setMethod ("%*%", signature (x = "hyperSpec", y = "hyperSpec"),
           function (x, y){
             validObject (x)
             validObject (y)

             if (ncol(y) > 1)
               warning(paste("Dropping column(s) of y:", paste(colnames(y$..),
                                                               collapse = ", ")))

             x@data$spc <-  x@data$spc %*% y@data$spc
             .wl (x) <- y@wavelength
             x@label$.wavelength = y@label$.wavelength

             .logentry (x, short = "%*%", long = as.character (y))
           }
           )

##' @noRd
setMethod ("%*%", signature (x = "hyperSpec", y = "matrix"),
           function (x, y){
             validObject (x)
             x@data$spc <-  x@data$spc %*% y
             .wl (x) <- seq_len (ncol (y))
             x@label$.wavelength = NA
             .logentry (x, short = "%*%", long = list (y = .paste.row (y, val = TRUE)))
           }
           )

##' @noRd
setMethod ("%*%", signature (x = "matrix", y = "hyperSpec"),
           function (x, y){
             validObject (y)

             if (ncol(y) > 1)
               warning(paste("Dropping column(s) of y:", paste(colnames(y$..),
                                                               collapse = ", ")))
             y <- new ("hyperSpec",
                       wavelength = y@wavelength,
                       spc = x %*% y@data$spc,
                       log = y@log
                       )

             .logentry (y, short = "%*%", long = list (x = .paste.row (x, val = TRUE)))
           }
           )
