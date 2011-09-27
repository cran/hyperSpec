##' @nord
if (!require (svUnit))
  `test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
 }

