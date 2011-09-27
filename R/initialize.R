###-----------------------------------------------------------------------------
###
###  initialize -- initialization, called by new ("hyperSpec", ...)
###
###  C. Beleites
###

##' @include paste.row.R
##' @nord
.initialize <- function (.Object, spc = NULL, data = NULL, wavelength = NULL, labels = NULL, log = NULL,
                         ## ...,
                         short = "initialize", user = NULL, date = NULL){

  
  if (is.null (log) && .options$log)    # avoid if no log is needed: involves copy of data
    long <- list (data       = if (missing (data))       "missing" else .paste.row (data, val = TRUE),
                  spc        = if (missing (spc))        "missing" else .paste.row (spc, val = TRUE,
                    range = FALSE),
                  wavelength = if (missing (wavelength)) "missing" else wavelength,
                  labels      = if (missing (labels))      "missing" else labels)
  else
    long <- list ()

  ## do the small stuff first, so we need not be too careful about copies

  ## the wavelength axis
  if (!is.null (spc)){
    if (is.null (dim (spc))){
      nwl <- length (spc)
      if (.options$gc) gc ()
      dim (spc) <- c(1, nwl)
      if (.options$gc) gc ()
    } else {
      nwl <- ncol (spc)
    }
  } else if (!is.null (data$spc))
    nwl <- ncol (data$spc)
  else
    nwl <- 0

  
  if (is.null (wavelength)){
    ## guess from spc's colnames
    if (!is.null (spc))
      wavelength <- as.numeric (colnames (spc))
    
    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- as.numeric (colnames (data$spc))
    
    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- seq_len (nwl) # guessing didn't work
  } 
  .Object@wavelength <- wavelength
  
  ## column + wavelength axis labels
  if (is.null (labels) || length (labels) == 0L){
    cln <- c (colnames (data), '.wavelength')
    if (! any (grepl ("spc", cln)))
      cln <- c (cln, "spc")
    labels <- vector ("list", length (cln))
    names (labels) <- cln
    rm (cln)
  }
  
  ## transform labels into expressions
  .make.expression <- function (x){	
    if (is.language (x) && ! is.expression (x))
      class (x) <- "expression"
    else if (is.character (x))
      x <- as.expression (x)
    x
  }

  labels <- lapply (labels, .make.expression)
  
  .Object@label <- labels

  rm (labels, wavelength)
  if (.options$gc) gc ()
  
  ## even the logbook may be given...
  if (is.data.frame (log)) {
    .Object@log <- log
  } else {
    .Object@log <- data.frame ()
    if (is.null (log))
      log <- list (short = short, long = long, user = user, date = date)
    
    .Object <- .logentry (.Object, .entry = log)
  }
  rm (log)
  if (.options$gc) gc ()
        
  if (! is.null (data$spc) && ! (is.null (spc)))
    warning ("Spectra in data are overwritten by argument spc.")
  
  ## deal with spectra
  if (is.null (spc) && is.null (data$spc)){
    spc <- structure(numeric (0), .Dim = c(0L, 0L))
  } 

  if (! is.null (spc) && !is.matrix (spc)) {
    spc <- structure (spc,
                      dim = c (1L, length (spc)), # use spc as row vector


##' Dimnames of an hyperSpec Object
##' \code{rownames} yields the names of the rows (spectra) of a
##' \code{hyperSpec} object,
##' 
##' \code{colnames} the names of the data colums, and
##' 
##' \code{dimnames} both together with the names of the wavelengths (see
##' below).
##' 
##' \code{dimnames} returns in element \code{wl} the colnames of the spectra
##' matrix, \emph{not} the values.
##' 
##' The replacement functions for column and row names are used like:
##' \preformatted{ colnames (x) <- value rownames (x) <- value }
##' 
##' If changing the colnames, be careful not to touch \code{spc}. Otherwise an
##' error about an invalid hyperSpec object results.
##' 
##' \code{colnames<-} renames the respective labels as well.
##' 
##' @name dimnames
##' @aliases dimnames,hyperSpec-method colnames,hyperSpec-method
##'   rownames,hyperSpec-method colnames<-,hyperSpec-method
##'   rownames<-,hyperSpec-method
##' @docType methods
##' @param x a \code{hyperSpec} object
##' @param do.NULL,prefix handed to \code{\link[base]{rownames}} and
##'   \code{\link[base]{colnames}}, respectively.
##' @return \code{rownames} and \code{colnames} return an \code{character}
##'   vector,
##' 
##' \code{dimames} returns a list with elements \code{row}, \code{data},
##'   \code{wl} with the respective character vectors.
##' @author C. Beleites
##' @seealso \code{\link[base]{rownames}}, \code{\link[base]{colnames}}, and
##'   \code{\link[base]{dimnames}}
##' 
##' \code{\link[base]{rownames<-}} and \code{\link[base]{colnames<-}} for the
##'   replacement functions
##' 
##' \code{\link[hyperSpec]{wl}}, \code{\link[hyperSpec]{wl<-}} for accessing
##'   the values of the wavelength axis.
##' @keywords methods
##' @export
##' @examples
##' 
##' rownames (flu)
##' colnames (flu)
##' dimnames (laser)
##' 
##' rownames (flu) <- paste ("sample", LETTERS [1 : 6])
##' flu$..
##' 
                      dimnames = list (NULL, names (spc))) 
  } 
  if (.options$gc) gc ()

  if (! is.null (spc)){
    attr (spc, "class") <- "AsIs"       # I seems to make more than one copy
    if (.options$gc) gc ()
  }
  
  ## deal with extra data
  if (is.null (data)){
    data <- data.frame (spc = spc)
  } else if (is.null (data$spc)){
    data$spc <- spc
  }
  rm (spc)
  if (.options$gc) gc ()
  
  attr (data$spc, "class") <- NULL      # more than one copy!?
  if (.options$gc) gc ()

  .Object@data <- data
  if (.options$gc) gc ()
  
  ## finally: check whether we got a valid hyperSpec object
  validObject (.Object)

  .Object
}

##' Creating a hyperSpec Object
##' Like other S4 objects, a hyperSpec object can be created by \code{new}. The
##' hyperSpec object is then \code{initialize}d using the given parameters.
##' 
##' If option \code{gc} is \code{TRUE}, the initialization will have frequent
##' calls to \code{gc ()} which can help to avoid swapping or running out of
##' memory.
##' 
##' @name initialize
##' @rdname initialize
##' @aliases initialize,hyperSpec-method initialize create
##'   create,hyperSpec-method new,hyperSpec-method new
##' @docType methods
##' @param .Object the new \code{hyperSpec} object.
##' @param data \code{data.frame}, possibly with the spectra in
##'   \code{data$spc}, and further variates in more columns.  A matrix can be
##'   entered as \emph{one} column of a data frame by: \code{data.frame (spc =
##'   I (as.matrix (spc)))}.
##' 
##' However, it will usually be more convenient if the spectra are given in
##'   \code{spc}
##' @param spc the spectra matrix.
##' 
##' \code{spc} does not need to be a matrix, it is converted explicitly by
##'   \code{I (as.matrix (spc))}.
##' @param wavelength The wavelengths corresponding to the columns of
##'   \code{data}. If no wavelengths are given, an appropriate vector is
##'   derived from the column names of \code{data$spc}. If this is not
##'   possible, \code{1 : ncol (data$spc)} is used instead.
##' @param label A \code{list} containing the labels for the columns of the
##'   \code{data} slot of the \code{hyperSpec} object and for the wavelength
##'   (in \code{label$.wavelength}). The labels should be given in a form ready
##'   for the text-drawing functions (see \code{\link[grDevices]{plotmath}}).
##' 
##' If \code{label} is not given, a list containing \code{NULL} for each of the
##'   columns of\code{data} and \code{wavelength} is used.
##' @param log A \code{list} used to fill into the first entry of
##'   \code{.Object@@log}. The elements \code{log$short}, \code{log$long},
##'   \code{log$date}, and \code{log$user} are handed down to
##'   \code{\link{logentry}}.
##' @author C.Beleites
##' @seealso \code{\link[methods]{new}} for more information on creating and
##'   initializing S4 objects.
##' 
##' \code{\link[grDevices]{plotmath}} on expressions for math annotations as
##'   for slot \code{label}.
##' 
##' \code{\link{hy.setOptions}}
##' @include utils.R
##' @keywords methods datagen
##' @examples
##' 
##' new ("hyperSpec")
##' 
##' spc <- matrix (rnorm (12), ncol = 4)
##' new ("hyperSpec", spc = spc)
##' new ("hyperSpec", data = data.frame (x = letters[1:3]),
##'      spc = spc)
##' 
##' colnames (spc) <- 600:603
##' new ("hyperSpec", spc = spc)  # wavelength taken from colnames (spc)
##' 
##' # given wavelengths precede over colnames of spc
##' new ("hyperSpec", spc = spc, wavelength = 700:703)
##' 
##' # specifying labels
##' h <- new ("hyperSpec", spc = spc, data = data.frame (pos = 1 : 3),
##'           label = list (spc = "I / a.u.",
##'                         .wavelength = expression (tilde (nu) / cm^-1),
##'                         pos = expression ("/" (x, mu*m)))
##' )
##' 
##' plot (h)
##' plotc (h, spc ~ pos)
##' 
##' # giving a log entry
##' new ("hyperSpec", log = list (short = "measurement parameters",
##'      long = list ("exposure / s" = 10, sample = "327a")))
##' 
setMethod ("initialize", "hyperSpec", .initialize)

test (.initialize) <- function (){

  checkEqualsNumeric (dim (new ("hyperSpec")), c (0L, 1L, 0L))

  h <- new ("hyperSpec", spc = 1 : 4)
  checkEqualsNumeric (h@data$spc, 1 : 4)
  checkEqualsNumeric (dim (h), c (1L, 1L, 4L))
  checkEqualsNumeric (h@wavelength, 1 : 4)
  

  spc <- matrix (c(1 : 12), nrow = 3)
  h <- new ("hyperSpec", spc = spc)
  checkEqualsNumeric (h@data$spc, spc)
  checkEqualsNumeric (dim (h), c (3L, 1L, 4L))
  checkEqualsNumeric (h@wavelength, 1 : 4)

  colnames(spc) <- c(600, 601, 602, 603)
  h <- new ("hyperSpec", spc = spc)
  checkEqualsNumeric (h@data$spc, spc)
  checkEqualsNumeric (dim (h), c (3L, 1L, 4L))
  checkEqualsNumeric (h@wavelength, c(600, 601, 602, 603))
}
