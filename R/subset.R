
.subset <- function (x, ..., short = "subset", date = NULL, user = NULL){
  validObject (x)
  x@data <- subset (x@data, ...)
  validObject (x)

  long <- match.call (call = sys.call(sys.parent(1)))
  long <- as.character (long) [-(1:2)]
  .logentry (x, long = long, short = short, date = date, user = user)  
}


##' subset for hyperSpec object
##'
##' @title subset
##' @param x hyperSpec object
##' @param \dots handed to \code{\link[base]{subset}} (data.frame method)
##' @param short,user,date passed to \code{\link[hyperSpec]{logentry}}
##' @docType methods
##' @return hyperSpec object containing the respective subset of spectra.
##' @author Claudia Beleites
##' @seealso \code{\link[base]{subset}}
##' @export 
setMethod ("subset", signature = signature (x = "hyperSpec"), .subset)
