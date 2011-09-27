##' hyperSpec unit tests
##' If \code{\link[svUnit]{svUnit}} is available, run the unit tests and
##' display the results.
##' 
##'
##' @rdname unittests
##' @return \code{NA} if \code{\link[svUnit]{svUnit}} is not available,
##'   otherwise \code{TRUE} if all tests are passed successfully. If a test
##'   fails, \code{hy.unittest} stops with an error.
##' @author C. Beleites
##' @seealso \code{\link[svUnit]{svUnit}}
##' @keywords programming utilities
##' @export
##' @examples
##' 
##' if (require (svUnit)){
##'   hy.unittest ()
##' }
##' 
hy.unittest <- function (){
  if (! require (svUnit)){
    warning ("svUnit required to run the unit tests.")
    return (NA)
  }

  tests <- unlist (eapply (env = getNamespace ("hyperSpec"), FUN = is.test, all.names = TRUE))
  tests <- names (tests [tests])

  tests <- sapply (tests, get, envir = getNamespace ("hyperSpec"))

  clearLog ()
  for (t in seq_along (tests)) {
    runTest (tests [[t]], names (tests) [t])
  }
  print (stats (Log()))

  errorLog (summarize = FALSE)
  invisible (TRUE)
}
