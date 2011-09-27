##' Logging the processing ot a hyperSpec Object
##' Extracts the slot \code{@@log} of a \code{hyperSpec} object.
##' 
##' A \code{data.frame} in slot \code{x@@log} keeps track of the changes done to
##' the \code{hyperSpec} object.
##' 
##' If option \code{log} is \code{TRUE}, entries will be generated
##' automatically by hyperSpec functions that do assignments (see
##' \code{\link{hy.setOptions}}). Entries can also be created manually via
##' \code{\link{logentry}}.
##' 
##' @param x a \code{hyperSpec} object
##' @return a \code{data.frame} containing \code{x@@log}
##' @author C. Beleites
##' @seealso \code{\link{hy.setOptions}}, \code{\link{logentry}}.
##' @export
##' @examples
##' 
##' logbook (flu)
##' 
logbook <- function (x){
  chk.hy (x)
  validObject (x)

  x@log
}

