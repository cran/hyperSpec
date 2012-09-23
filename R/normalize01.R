## @noRd
setGeneric ("normalize01", function (x, ...) standardGeneric ("normalize01"))

##' Normalize numbers -> [0, 1]
##'
##' The input \code{x} is mapped to [0, 1] by subtracting the minimum and subsequently dividing by
##' the maximum. If all elements of \code{x} are equal, 1 is returned.
##' 
##' @param x  vector with values to transform
##' @param eps tolerance level for determining what is 0 and 1
##' @param \dots additional parameters such as \code{eps} handed down.
##' @return vector with \code{x} values mapped to the interval [0, 1]
##' @author C. Beleites
##' @seealso \code{\link[hyperSpec]{wl.eval}}, \code{\link[hyperSpec]{vanderMonde}}
##' @export 
setMethod (normalize01, signature (x = "matrix"), function (x, eps = .Machine$double.eps){
  m <- apply (x, 1, min)
  x <- sweep (x, 1, m, `-`)
  m <- apply (x, 1, max)
  x <- sweep (x, 1, m, `/`)
  x [m < eps, ] <- 1
  x
})

##' @rdname normalize01
setMethod ("normalize01", signature (x = "numeric"), function (x, eps = .Machine$double.eps){
  x <- x - min (x)

  m <- max (x)
  if (m < eps)
    rep (1, length (x))
  else
    x / m
})

##' @rdname normalize01
setMethod (normalize01, signature (x = "hyperSpec"), function (x, ...){
  validObject (x)

  x@data$spc <- normalize01 (unclass (x@data$spc), ...)

  ## logbook
  x
})
           
.test (normalize01) <- function (){
  x <- runif (10, min = -1e3, max = 1e3)
  tmp.x <- normalize01 (x)

  checkEqualsNumeric (min (tmp.x), 0)
  checkEqualsNumeric (max (tmp.x), 1)
  checkEqualsNumeric (tmp.x, (x - min (x)) / diff (range (x)))

  ## constant => 1
  checkEqualsNumeric (normalize01 (rep (1, 3)), rep (1, 3))

  ## matrix method
  m <- rbind (x, 2)
  tmp.m <- normalize01 (m)

  checkEqualsNumeric (tmp.m, rbind (tmp.x, 1))

  ## hyperSpec method
  tmp.hy <- normalize01 (vanderMonde (flu, 1))
}
