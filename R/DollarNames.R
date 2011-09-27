##' command line completion for $
##' 
##' @aliases .DollarNames.hyperSpec
##' @author C. Beleites
##' @seealso \code{\link[utils]{.DollarNames}}
##' @export 
##' @callGraph
##' @keywords utilities
##' @title command line completion for $
##' @param x the hyperSpecobject
##' @param pattern pattern to look for
##' @return the name of the extra data slot
.DollarNames.hyperSpec <- function (x, pattern)
  grep (pattern, colnames (x@data), value = TRUE)
