##' @rdname math
##' @aliases log log,hyperSpec-method
##' @export
setMethod ("log", signature (x = "hyperSpec"),
           function (x, base = exp (1), ...,
                     short = "log", user = NULL, date = NULL){
             validObject (x)

             x [[]] <-  log (x[[]], base = base)
             .logentry (x, short = "log", long = list (base = base))
           }
           )
