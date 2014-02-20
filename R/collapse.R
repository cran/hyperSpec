##' collapse/bind several hyperSpec objects into one object
##'
##' The spectra from all objects will be put into one object.
##' The resulting object has all wavelengths that occur in the input objects.
##' Data points corresponding to wavelengths not in the original spectrum will be set to NA.
##' Extra data is combined in the same manner.
##' 
##' @author C. Beleites
##' @title Collapse hyperSpec objects
##' @export
##' @param \dots hyperSpec objects to be collapsed into one object. Instead of giving several
##' arguments, a list with all objects to be collapsed may be given.
##' @param short.log,short,user,date deprecated
##' @aliases collapse collapse.hyperSpec
##' @seealso \code{\link[base]{merge}} to merge hyperSpec objects that share wavelengths but contain different spectra,  \code{\link[base]{rbind}}, and  \code{\link[plyr]{rbind.fill}} for 
##' @return a hyperSpec object
##' @keywords manip
##' @examples
##' barbiturates [1:3]
##' barb <- collapse (barbiturates [1:3])
##' barb
##' 
##' a <- barbiturates [[1]]
##' b <- barbiturates [[2]]
##' c <- barbiturates [[3]]
##' 
##' a
##' b
##' c
##' collapse (a, b, c)
##' 

collapse <- function (..., short.log = TRUE, short = "collapse", user = NULL, date = NULL){
  dots <- list (...)

  ## accept also a list of hyperSpec objects
  if (length (dots) == 1 && is.list (dots [[1]]))
    dots <- dots [[1]]

  ## check the arguments
  lapply (dots, chk.hy)
  lapply (dots, validObject)

  logs <- list()

  ## prepare log ?
  if (hy.getOption ("log")){

    if (short.log)
      logs <- paste ("hyperSpec [",
                     unlist (lapply (dots, function (x) paste (dim (x), collapse = " x "))),
                     "]", sep = "")
    else
      logs <- unlist (lapply (dots, function (x) paste (as.character (x, range = FALSE), "\n", collapse = "")))

    logs <- list (short = short, long = logs, user = user, date = date)
  } else {
    logs <- NULL
  }

  ## prepare new labels
  labels <- unlist (lapply (dots, slot, "label"))
  labels <- labels [unique (names (labels))]
  
  ## merge data & spectra matrices
  dots <- lapply (dots, .wl2cln)
  dots <- rbind.fill (lapply (dots, slot, "data"))
  wl <- as.numeric (colnames (dots$spc))

  ## make a new hyperSpec object
  x <- new ("hyperSpec", wavelength = wl, data = dots, labels = labels,
            log = logs)
  
  x
}

.wl2cln <- function (x){
  colnames (x@data$spc) <- formatC (x@wavelength, digits = 17)
  x
}

