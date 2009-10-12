## wishlist: slice_map
## wishlist: parse arg of form x..y = z => x = list (y = z) 

.onLoad <- function (libname, pkgname){
  require (lattice) 
  require (utils) 
}

.onAttach <- function (libname, pkgname){
  desc <- utils::packageDescription("hyperSpec")
  vers <- paste("V. ", desc$Version)
  cat ("Package ",  desc$Package, ", version ", desc$Version, "\n\n",
       "To get started, try\n",
       '   vignette ("introduction", package = "hyperSpec")\n',
       '   package?hyperSpec \n',
       '   vignette (package = "hyperSpec")\n\n',
       "If you use this package please cite it appropriately.\n",
       "   citation(\"hyperSpec\")\nwill give you the correct reference.", "\n\n",
       'The project is hosted on http://r-forge.r-project.org/projects/hyperspec/\n\n',
       sep = "")  
}
setClass ("hyperSpec",
          representation = representation (
            wavelength = "numeric",     # spectral abscissa
            data = "data.frame",        # data: spectra & information related to each spectrum
            label = "list",             # labels and units of the stored 
            log = "data.frame"          # log of transformations etc.
            ),
          prototype = prototype (
            wavelength = numeric (0),
            data = data.frame (spc = I(matrix(NA, 0, 0))),
            label = list (.wavelength = NULL, "spc" = NULL),
            log = data.frame (short.description = character (0),
              long.description = I(list ()),
              date = numeric (0), 
              user = character (0)
              )), 
          validity = function (object){
            ncol <- ncol (object@data$spc)
            if (is.null (ncol))
              ncol <- 0
            if (length (object@wavelength) != ncol)
              return ("Length of wavelength vector differs from number of data points per spectrum.")
            
            if (any (is.na (match (colnames (object@log),
                                   c("short.description", "long.description",  "date", "user")))))
              return ("Slot log does not have the correct columns.")
            
            TRUE
          }
          )

###-----------------------------------------------------------------------------
###
### .is.hy - checks whether the object is a hyperSpec object
### to be used like validObject
### 

.is.hy <- function (x){
  if (! is (x, "hyperSpec"))
    stop ("wl<- works on hyperSpec objects only")
  
  TRUE
}

###-----------------------------------------------------------------------------
###
###  initialize -- initialization, called by new ("hyperSpec", ...)
###
setMethod ("initialize", "hyperSpec",
           function (.Object, spc = NULL, data = NULL,
                     wavelength = NULL, label = NULL, log = NULL){
             long <- list ()
             
             if (is.null (data))
               .Object@data <- data.frame (spc = I (matrix (NA, 0, 0)))
             else {
               .Object@data <- data
               long$data <- .paste.row (.Object@data, val = TRUE)
             }
             
             if (!is.null (spc)){
               if (is.numeric (spc) && !is.matrix (spc))
                 spc <- as.matrix (t (spc)) # make a 1 row matrix
               if (nrow (.Object) == nrow (spc)){
                 if (!is.null (.Object@data$spc))
                   warning ("data$spc replaced by spc.")
                 .Object@data$spc <- I (as.matrix (spc))
               } else if (nrow (.Object) == 0)
                 .Object@data <- data.frame (spc = I (as.matrix (spc)))
               else
                 stop ("data and spc need to have the same number of rows.")
               long$spc <- .paste.row (spc, val = TRUE)
             }
             
             if (is.null (wavelength)){
               if (is.null (colnames (.Object@data$spc)))
                 colnames (.Object@data$spc) <- seq_len (ncol (.Object@data$spc))
               .wl(.Object) <- as.numeric (colnames (.Object@data$spc))
             } else {
               .wl(.Object) <- wavelength
               long$wavelength <- wavelength
             }
             if (any (is.na (.Object@wavelength)))
               .Object@wavelength <- seq_len (ncol (.Object@data$spc))
             
             if (is.null (label)){
               .Object@label <- vector ("list", length (colnames (.Object@data)) + 1)
               names (.Object@label) <- c(".wavelength", colnames (.Object@data))  
             } else{
               .Object@label <- label
               long$label <- label
             }
             
             if (is.null (log))
               .Object@log <- .logentry (.Object, short = "initialize", long = long)
             else
               if (is.data.frame(log))
                 .Object@log <- log
               else
                 .Object@log <- .logentry (.Object, short = log$short, long = log$long,
                                          date = log$date, user = log$user)
             
             validObject (.Object)
             
             .Object
           })

###-----------------------------------------------------------------------------
###
###  orderwl - order the wavelength axis ascending
###
orderwl <- function (x, na.last = TRUE, decreasing = FALSE,
                     short = "orderwl", date = NULL, user = NULL){
  ord <- order (x@wavelength, na.last = na.last, decreasing = decreasing)
  if (any (ord != seq_len (length (x@wavelength)))){
    x@data$spc <-  x@data$spc [, ord, drop = FALSE]
    .wl(x) <- x@wavelength [ord]
  }
  
  x@log <- .logentry (x, short = short,
                     long = list (na.last = na.last, decreasing = decreasing),
                     date = date, user = user) 
  x
}

###-----------------------------------------------------------------------------
###
###  print
###

setMethod ("print", "hyperSpec", function (x, log = FALSE, ...){
  validObject (x)
  cat (as.character (x, log = log, ...), sep ="\n")
  invisible (x)
})

###-----------------------------------------------------------------------------
###
###  show
###

setMethod ("show", "hyperSpec", function (object){
  print (object)
  invisible (NULL)
})
###-----------------------------------------------------------------------------
###
###  summary 
###

setMethod ("summary", "hyperSpec", function (object, log = TRUE, ...){
  print (object, log = log, ...)
})

###-------------------------------------------------------------------------------
###
###  .logentry - create new log item (hyperSpec)
###
###
.logentry <- function (x, short = NULL, long = NULL, date = NULL, user = NULL){
  validObject (x)
  
  Call <- sys.call (-1);
  
  if (is.null (short))
    short <- Call [[1]]
  
  if (is.null (long))
    long <- list (Call)
  else if (!is.list (long))
    long <- list (long)
  
  if (is.null (date)) 
    date <- Sys.time ()
  
  if (is.null (user)) 
    user <- paste (Sys.info()[c("user", "nodename")], collapse= "@")
  
  rbind (x@log, data.frame (short.description = as.character (short),
                            long.description = I(list (long)),
                            date = date,
                            user = user
                            )
         )
}

###-----------------------------------------------------------------------------
###
### .extract - internal function doing the work for extracting with [] and [[]]
###


.extract <- function (x, i, j, l, 
                      ...,
                      wl.index = FALSE
                      ){
  if (! missing (i))
    x@data <- x@data[i,, drop = FALSE]
  
  if (!missing (j)) 
    x@data <- x@data[, j, drop = FALSE]
  
  if (!missing (l)) {
    if (is.null (x@data$spc))
      warning ("Selected columns do not contain specta. l ignored.")
    else {
      if (!wl.index)
        l <- wl2i (x, l)
      
      x@data$spc <- x@data$spc[,l, drop = FALSE]
      .wl (x) <- x@wavelength[l]
    }
  }
  
  x
}

###-----------------------------------------------------------------------------
###
### extractsquare - extracting with []
###

setMethod ("[", "hyperSpec", function (x, i, j, l, #
                                       ...,
                                       wl.index = FALSE,
                                       short = NULL,
                                       drop = FALSE
                                       ){
  if (drop)
    warning ("Ignoring drop = TRUE.")
  
  validObject (x)
  
  env <- parent.frame (1) 
  
  x@log <- .logentry (x,
                     short = if (is.null (short))
                     "["
                     else
                     paste ("[ (", short, ")", sep =""), 
                     long = list (
                       i = if (missing (i))
                       ""
                       else
                       deparse (substitute (i, env)), 
                       j = if (missing (j))
                       ""
                       else
                       deparse (substitute (j, env)),
                       l = if (missing (l))
                       ""
                       else
                       deparse (substitute (l, env)),
                       wl.index = wl.index,
                       data = data))
  
  x <- .extract (x, i, j, l, ..., wl.index = wl.index)
  
  if (is.null (x@data$spc)){ 
    x@data$spc <- matrix (NA, nrow (x@data), 0)
    x@wavelength <- numeric (0)
  }
  
  x
})



###-----------------------------------------------------------------------------
###
### extractname - extracting with $
###

setMethod ("$", "hyperSpec", function (x, name){
  validObject (x)
  if (name == ".") ## shortcut
    x@data [, , drop = FALSE]
  else if (name == "..") 
    x@data[, -match ("spc", colnames (x@data)), drop = FALSE] 
  else
    x@data[[name]]
})


###-----------------------------------------------------------------------------
###
### extractsqsq - extracting with [[
###
setMethod ("[[", "hyperSpec", function (x, i, j, l, ...,
                                        wl.index = FALSE,
                                        drop = FALSE){
  validObject (x)
  
  x <- .extract (x, i, j, l, ..., wl.index = wl.index)
  
  if (missing (j))
    unclass (x@data$spc[,, drop = drop]) # removes the "AsIs"
  else {
    x@data[,, drop = drop]
  }
})


###-----------------------------------------------------------------------------
###
### as.data.frame
###
setMethod ("as.data.frame",
           signature (x = "hyperSpec", row.names = "missing", optional = "missing"),
           function (x, ...){
             validObject (x)
             x@data
           })

###-----------------------------------------------------------------------------
###
### as.long.df
###

as.long.df <- function (x, rownames = FALSE) {
  .is.hy (x)
  validObject (x)
  ispc <- match ("spc", colnames (x@data))

  dummy <- x@data [rep (seq (nrow (x)), nwl (x)), -ispc, drop = FALSE]

  dummy <- cbind (data.frame (wavelength = rep (x@wavelength, each = nrow (x)),
                              spc = as.numeric (x [[]])),
                  dummy)
  
  if (rownames)
    dummy <- data.frame (.rownames = as.factor (rep (rownames (x), nwl (x))),
                    dummy)

  dummy
}


###-----------------------------------------------------------------------------
###
### as.matrix
###
setMethod ("as.matrix", "hyperSpec", function (x, ...){
  validObject (x)
  x@data$spc
})

###-----------------------------------------------------------------------------
###
###  .paste.row
###  
###  
.paste.row <- function (x, label = "", name = "", ins = 0, i = NULL, val = FALSE, range = TRUE,
                        digits = getOption ("digits"), max.print = 5, shorten.to = c (2,1)){
  if (is.null (name)) name = ""
  if (is.null (label)) label = ""
  
  dummy <- ""
  
  if (val){
    if (is.list (x)){ 
      dummy <- paste ("", "columns/entries", paste (names (x), collapse = ", ")) 
    } else {
      if (range)
        val <- sort (unique (as.vector (x)))
      else
        val <- x
      if (length (val) > max.print)
        dummy <- c(
                   format (val [seq_len (shorten.to[1])], # was 1 :
                           digits = digits, trim = TRUE),
                   "...",
                   format (val [-seq_len (length (val) - shorten.to[2])], # was 1 :
                           digits = digits, trim = TRUE)
                   )
      else
        dummy <- format (val, digits = digits, trim = TRUE)
      dummy <- paste ("", if(range) "range", paste (dummy, collapse = " "), if(any (is.na (x))) "+ NA", collapse = " ")
      
    }
  }
  dummy <- paste (paste (rep (" ", ins), collapse = ""),
                  if (!is.null (i)) paste ("(", i, ") ", sep =""),
                  name,
                  if (nchar (name) != 0) ": ",
                  label,
                  if (nchar (label) != 0) " ",
                  if ((nchar (dummy) > 0) | (nchar (label) + nchar (name) > 0) | !is.null (i)) "[",
                  paste (class (x), collapse = ", "),
                  " ",
                  if (! is.null (dim (x)))
                  paste (if (is.matrix (x) & all (class (x) != "matrix")) "matrix " else
                         if (is.array (x) & all (class (x) != "array") & all (class (x) != "matrix")) "array ",
                         paste (dim (x), collapse = " x ")
                         , sep = "")
                  else if (length (x) > 1)
                  length (x),
                  if ((nchar (dummy) > 0) | (nchar (label) + nchar (name) > 0) | !is.null (i)) "]",
                  dummy,
                  sep ="")
  dummy
}
###-----------------------------------------------------------------------------
###
###  as.character
###  
###  

setMethod (as.character, "hyperSpec", function (x,
                                                digits = getOption ("digits"),
                                                max.print = 5,
                                                shorten.to = c(2,1),
                                                log = TRUE){#, ...){
  
  
  ## input checking
  validObject (x)
  
  if (is.null (max.print))
    max.print <- getOption ("max.print")
  
  if ((length (max.print) != 1) | ! is.numeric (max.print))
    stop ("max.print needs to be a number")
  if ((length (shorten.to) < 1) |(length (shorten.to) > 2) | ! is.numeric (shorten.to))
    stop ("shorten.to needs to be a numeric vector with length 1 or 2")
  if (sum (shorten.to) > max.print)
    stop ("sum (shorten.to) > max.print: this does not make sense.")
  
  ## printing information
  chr <- c("hyperSpec object",
           paste ("  ", nrow (x), "spectra"),
           paste ("  ", ncol (x), "data columns"),
           paste ("  ", nwl (x), "data points / spectrum")
           )
  
  chr <- c (chr, paste ("wavelength:",
                        .paste.row (x@wavelength, x@label$.wavelength, ins = 0, val = TRUE, 
                                    range = FALSE, shorten.to = shorten.to, max.print = max.print),
                        collapse = " ")
            )
  
  n.cols <- ncol (x@data)
  
  chr <- c(chr, paste ("data: ",
                       " (", nrow(x@data), " rows x ", n.cols, " columns)", sep = ""))
  if (n.cols > 0)
    for (n in names (x@data))
      chr <- c(chr, .paste.row (x@data[[n]], x@label[[n]], n, ins = 3,
                                i = match (n, names (x@data)), val = TRUE, 
                                shorten.to = shorten.to, max.print = max.print))
  
  if (log){
    chr <- c(chr, "log:")
    
    long <- lapply (as.character (x@log$long.description),
                    function (x, max.print, shorten.to, desc = TRUE){
                      if (nchar (x) > max.print)
                        paste (substr (x, 1, max.print), "...", sep = "")
                      else
                        x
                    },
                    max.print, shorten.to, desc = FALSE)
    width = c (4 + floor (log10 (nrow (x@log))),
      max (sapply (c("short", as.character (x@log$short.description)), nchar)),
      max (sapply (c("long", long), nchar)),
      max (sapply (c("date", as.character (x@log$date)), nchar)),
      max (sapply (c("user", as.character (x@log$user)), nchar))
      )
    
    chr <- c (chr, paste (paste (rep (" ", width [1]), collapse = ""),
                          format ("short", justify = "right", width = width [2]),
                          format ("long", justify = "right", width = width [3]),
                          format ("date", justify = "right", width = width [4]),
                          format ("user", justify = "right", width = width [5]),
                          sep = "   ")
              )
    
    for (i in seq_len (nrow (x@log)))
      chr <- c (chr, paste (format (i, justify = "right", width = width [1]),
                            format (as.character (x@log[i, 1]), justify = "right", width = width [2]),
                            format (long [i], justify = "right", width = width [3]),
                            format (as.character (x@log[i, 3]), justify = "right", width = width [4]),
                            format (as.character (x@log[i, 4]), justify = "right", width = width [5]),
                            sep = "   ")
                )
  }	
  chr
})


###-----------------------------------------------------------------------------
###
###  ncol
###  
###  

setMethod ("ncol", "hyperSpec", function (x){
  validObject (x)
  ncol (x@data) 
})


###-----------------------------------------------------------------------------
###
###  nrow
###  
###  

setMethod ("nrow", "hyperSpec", function (x){
  validObject (x)
  nrow (x@data) 
})

###-----------------------------------------------------------------------------
###
###  dim
###  
###  

setMethod ("dim", "hyperSpec", function (x){
  validObject (x)
  c (nrow = nrow (x@data), ncol = ncol (x@data), nwl = ncol (x@data$spc)) 
})

###-----------------------------------------------------------------------------
###
###  colnames
###  
###  

setMethod ("colnames", "hyperSpec", function (x, do.NULL = TRUE, prefix = "col"){
  validObject (x)
  colnames (x@data, do.NULL = do.NULL, prefix = prefix) 
})

###-----------------------------------------------------------------------------
###
###  colnames <-
###  
###  

setReplaceMethod ("colnames", "hyperSpec", function (x, value){
  validObject (x)
  colnames (x@data) <- value
  validObject (x)
  x
})


###-----------------------------------------------------------------------------
###
###  rownames
###  
###  

setMethod ("rownames", "hyperSpec", function (x, do.NULL = TRUE, prefix = "row"){
  validObject (x)
  rownames (x@data, do.NULL = do.NULL, prefix = prefix) 
})

###-----------------------------------------------------------------------------
###
###  rownames <-
###  
###  

setReplaceMethod ("rownames", "hyperSpec", function (x, value){
  validObject (x)
  rownames (x@data) <- value
  x
})

###-----------------------------------------------------------------------------
###
###  dimnames
###  
###  

setMethod("dimnames", "hyperSpec", function (x){
  validObject (x)
  list (row = rownames (x@data), data = colnames (x@data), wl = colnames (x@data$spc)) 
})
###-----------------------------------------------------------------------------
###
###  logbook
###  
###
logbook <- function (x){
  .is.hy (x)
  validObject (x)
  x@log
}
###-----------------------------------------------------------------------------
###
###  logentry
###  
###
logentry <- function (x, short = NULL, long = NULL, date = NULL, user = NULL){
  .is.hy (x)
  validObject (x)
  x@log <- .logentry (x, short, long, date, user)
}
###-----------------------------------------------------------------------------
###
###  labels
###  
###  

setMethod ("labels", "hyperSpec", function (object, which = NULL, drop = TRUE, ...){
  validObject (object)
  
  if (is.null (which))
    object@label
  else {
    label <- object@label [which]
    if (drop && (length (label) == 1))
      label <- label [[1]]
    label
  }
})

###-----------------------------------------------------------------------------
###
###  labels<-
###  
###  

"labels<-" <- function (object, which = NULL, ..., value){
  .is.hy (object)
  validObject (object)
  
  if (is.null (which))
    object@label <- value
  else {
    if ((is.character (which) && !which %in% colnames (object@data)) && which != ".wavelength" ||
        (is.numeric (which) && (which < 1 || which > ncol (object@data) + 1)) ||
        (is.logical (which) && length (which) != ncol (object@data) + 1)
        )
      stop ("Label does not exist!")
    
    object@label [[which]] <- value
    
  }
  
  object@log <- .logentry (object, short = "labels<-", 
                          long = list (which = which, value = value), ...)
  
  validObject (object) 
  
  object
}



###-----------------------------------------------------------------------------
###
###  nwl 
###  
###
nwl <- function (x){
  validObject (x)
  ncol (x@data$spc)
}

###-----------------------------------------------------------------------------
###
###  wl 
###  
###
wl <- function (x){
  validObject (x)
  x@wavelength
}

###-----------------------------------------------------------------------------
###
###  .wl 
###  
###
".wl<-" <- function (x, digits = 6, value){
  x@wavelength <- value
  colnames (x@data$spc) <- signif (value, digits)
  
  x
}

###-----------------------------------------------------------------------------
###
###  wl 
###  
###
"wl<-" <- function (x, label = NULL, digits = 6, short = "wl<-", user = NULL, date = NULL, value){
  
  .is.hy (x)
  validObject (x)
  
  if (is.numeric (value)){
    if (is.null (label))
      warning ("Do not forget to adjust the label of the wavelength axis.")
  } else if (is.list (value)){
    label <- value$label
    value <- value$wl
  }
  
  .wl (x) <- value
  
  x@label$.wavelength <- label
  
  validObject (x)
  x@log <- .logentry (x, short = short, long = list (value = value, digits = digits), 
                     date = date, user = user)
  
  x
}


###-----------------------------------------------------------------------------
###
### replacing with [<-
###

setReplaceMethod("[", "hyperSpec", function (x, i, j,
                                             short = NULL,
                                             ...,
                                             value){
  validObject (x)
  
  long <- list (i = if (missing (i)) "" else i ,
                j = if (missing (j)) "" else j,
                drop = drop,
                ...,
                value = if (is (value, "hyperSpec")) 
                as.character (value) 
                else 
                .paste.row (value, val = TRUE)
                ) 
  
  if (missing (i)) i <- seq_len (nrow (x@data))
  if (missing (j)) j <- seq_len (ncol (x@data))
  
  if (is (value, "hyperSpec")){
    validObject (value)
    x@data [i, j, ...] <- value@data
  } else
  x@data [i, j, ...] <- value
  
  x@log <- .logentry (x,
                     short = if (is.null (short)) "[<-"
                     else paste ("[<- (", short, ")" , sep = ""),
                     long = long
                     )
  
  validObject (x)
  
  x
})

###-----------------------------------------------------------------------------
###
### replacing with [[<-
###

setReplaceMethod ("[[", "hyperSpec", function (x, i, j, l, 
                                               wl.index = FALSE,
                                               short = NULL,
                                               ...,
                                               value){
  validObject (x)
  
  long <- list (i = if (missing (i)) "" else i ,
                l = if (missing (l)) "" else l,
                wl.index = wl.index,
                ...,
                value = if (is (value, "hyperSpec")) as.character (value)
                else .paste.row (value, val = TRUE)
                ) 
  
  if (! missing (j))
    stop ("The spectra matrix may only be indexed by i (spectra) and l (wavlengths). j (data column) must be missing.")
  
  if  (!missing (l) && !wl.index)
    l <- wl2i (x, l)
  
  if (is (value, "hyperSpec")){
    validObject (value)
    value <- value@data$spc
  } 
  
  x@data$spc[i, l, ...] <- value
  
  x@log <- .logentry (x,
                     short = if (is.null (short))
                     "[[<-"
                     else
                     paste ("[[<- (", short, ")" , sep = ""), 
                     long = long
                     )
  
  validObject (x)
  
  x
})



###-----------------------------------------------------------------------------
###
### subassignname - subassigning with $
###
setReplaceMethod ("$", "hyperSpec", function (x, name, value){
  validObject (x)
  
  if (is.list (value) && (length (value) == 2)){
    ilabel <- match ("label", names (value))
    if (is.na (ilabel))
      ilabel <- 2
    label <- value [[ilabel]]
    
    value <- value [[3 - ilabel]] ## the other of the 2 entries
  } else 
  label <- name
  
  if (name == "..") { ## shortcut
    i <- -match ("spc", colnames (x@data))
    x@data[, i] <- value
    
    if (!is.null (label)){
      i <- colnames (x@data)[i]
      i <- match (i, names (x@label))
      x@label[i] <- label
    }
  } else {
    dots <- list (x = x@data, name = name, value = value)
    x@data <- do.call("$<-", dots) 				
    x@label[[name]] <- label
  }
  
  x@log <- .logentry (x,
                     short = "$<-", 
                     long = list (name = name, value = .paste.row (value, val = TRUE))
                     )
  
  x
})

###-----------------------------------------------------------------------------
###
###  artihmetic functions
###  
###  

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)
             if (.Generic %in% c ("*", "^", "%%", "%/%", "/"))
               warning (paste ("Do you really want to use", .Generic, "on 2 hyperSpec objects?"))
             e1 [[]] <- callGeneric (e1[[]], e2[[]])
             e1@log <- .logentry (e1, short = .Generic, long = as.character (e2))
             e1
           }
           )

.arithx <- function (e1, e2){
  validObject (e1)
  if (missing (e2)){
    e1  [[]] <- callGeneric (e1 [[]])
    e1@log <- .logentry (e1, short = .Generic, long = list ())  
    
  } else {
    e1  [[]] <- callGeneric (e1 [[]], e2)
    e1@log <- .logentry (e1, short = .Generic, 
			long = list (e2 = .paste.row (e2, val = TRUE))) 
  }
  e1
}

.arithy <- function (e1, e2){ 
  validObject (e2)
  e2  [[]] <- callGeneric (e1, e2 [[]])
  e2@log <- .logentry (e2, short = .Generic, long = list (e1 = .paste.row (e1, val = TRUE)))
  e2
}

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "numeric"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "matrix"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "missing"), .arithx)

setMethod ("Arith", signature (e1 = "numeric", e2 = "hyperSpec"), .arithy)
setMethod ("Arith", signature (e1 = "matrix", e2 = "hyperSpec"), .arithy)

###-----------------------------------------------------------------------------
###
###  %*%
###  
###

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
             x@log <- .logentry (x, short = "%*%", long = as.character (y))
             x
           }
           )

setMethod ("%*%", signature (x = "hyperSpec", y = "matrix"),
           function (x, y){
             validObject (x)
             x@data$spc <-  x@data$spc %*% y
             .wl (x) <- seq_len (ncol (y)) 
             x@label$.wavelength = NA
             x@log <- .logentry (x, short = "%*%", long = list (y = .paste.row (y, val = TRUE)))
             x
           }
           )

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
             
             y@log <- .logentry (y, short = "%*%", long = list (x = .paste.row (x, val = TRUE)))
             y
           }
           )
###-----------------------------------------------------------------------------
###
###  compare functions
###  
###  

setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)
             
             callGeneric (e1[[]], e2[[]])
           }
           )

.compx <- function (e1, e2){
  validObject (e1)
  callGeneric (e1 [[]], e2)
}

.compy <- function (e1, e2){
  validObject (e2)
  callGeneric (e1, e2 [[]])
}

setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "numeric"), .compx)
setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "matrix"), .compx)

setMethod ("Compare", signature (e1 = "numeric", e2 = "hyperSpec"), .compy)
setMethod ("Compare", signature (e1 = "matrix", e2 = "hyperSpec"), .compy)
###-----------------------------------------------------------------------------
###
###  all.equal
###  
###  

setMethod ("all.equal", signature (target = "hyperSpec", current = "hyperSpec"),
           function (target, current, ..., check.column.order = FALSE, check.label = FALSE, check.log = FALSE){
             validObject (target)
             validObject (current)
             
             result <- character (0)
             
             cmp <- all.equal (target@wavelength, current@wavelength, ...)
             if (! isTRUE (cmp)) result <- c("@wavelength:", cmp)
             
             if (check.column.order)
               cmp <- all.equal (target@data, current@data, ...)
             else
               cmp <- all.equal ( target@data[order (colnames ( target@data))], 
                                 current@data[order (colnames (current@data))], ...)
             if (! isTRUE (cmp)) result <- c (result, "@data:", cmp)
             
             if (check.label){
               cmp <- all.equal (target@label[order (names (target@label))], 
                                 current@label[order (names (current@label))], ...)
               if (! isTRUE (cmp)) result <- c (result, "@label:", cmp)		
             }
             
             if (check.log) {
               cmp <- all.equal (target@log, current@log, ...)
               if (! isTRUE (cmp)) result <- c (result, "@log:", cmp)
             }
             
             if (length (result) == 0)
               TRUE
             else
               result 
           }
           )


###-----------------------------------------------------------------------------
###
###  math functions
###  
###  

setMethod ("Math", signature (x = "hyperSpec"),
           function (x){
             validObject (x)
             
             if (grepl ("^cum", .Generic) || grepl ("gamma$", .Generic))
               warning (paste ("Do you really want to use", .Generic, "on a hyperSpec object?"))
             
             x [[]] <- callGeneric (x[[]])
             x@log <- .logentry (x, short = .Generic, long = list())
             x
           }
           )


###-----------------------------------------------------------------------------
###
###  log
###  
###

setMethod ("log", signature (x = "hyperSpec"),
           function (x, base = exp (1)){
             validObject (x)
             
             x [[]] <-  log (x[[]], base = base) 
             x@log <- .logentry (x, short = "log", long = list (base = base))
             x
           }
           )

###-----------------------------------------------------------------------------
###
###  math functions
###  
###  

setMethod ("Math2", signature (x = "hyperSpec"),
           function (x, digits){
             validObject (x)
             
             x [[]] <- callGeneric (x[[]], digits)
             
             x@log <- .logentry (x, short = .Generic, 
                                long = list(if (exists ("digits")) digits = digits))
             x
           }
           )

###-----------------------------------------------------------------------------
###
###  summary functions
###  
###  

setMethod ("Summary", signature (x = "hyperSpec"),
           function (x, ..., na.rm = FALSE){
             validObject (x)
             
             if ((.Generic == "prod") || (.Generic == "sum"))
               warning (paste ("Do you really want to use", .Generic, "on a hyperSpec object?"))
             
             ## dispatch also on the objects in ...
             x <- sapply (list (x[[]], ...), .Generic, na.rm = na.rm) 
             
             callGeneric (x, na.rm = na.rm)
           }
           )

###-----------------------------------------------------------------------------
###
### cbind2
###  
###  
setMethod ("cbind2", signature (x = "hyperSpec", y  = "hyperSpec"),
           function (x, y){
             validObject (x)
             validObject (y)
             
             cols <- match (colnames (x@data), colnames (y@data))
             cols <- colnames(y@data)[cols]
             cols <- cols[!is.na (cols)]
             cols <- cols [-match ("spc", cols)]
             
             if (length (cols) < 0){
               ord <- do.call (order, x@data[, cols, drop = FALSE])
               x@data <- x@data[ord, , drop = FALSE]
               
               ord <- do.call (order, y@data[, cols, drop = FALSE])
               y@data <- y@data[ord, , drop = FALSE]
               
               if (any (x@data[, cols, drop = FALSE] != y@data[, cols, drop = FALSE]))
                 stop ("hyperSpec objects must have the same data (except data$spc)") 
             }
             
             x@data$spc <- cbind(x@data$spc, y@data$spc)
             .wl (x) <- c (x@wavelength, y@wavelength)
             
             x@data <- cbind (x@data,
                              y@data[, is.na (match (colnames (y@data), colnames (x@data))), drop = FALSE])
             
             x@log <- .logentry (x, short = "cbind2", long = as.character (y))
             x
           }
           )

setMethod("cbind2", signature (x = "hyperSpec", y = "missing"), function (x, y)x) 

###-----------------------------------------------------------------------------
###
### rbind2
###  
###  
setMethod("rbind2",
          signature(x = "hyperSpec", y = "hyperSpec"),
          function (x, y) {
            validObject (x)
            validObject (y)
            
            if (any (x@wavelength != y@wavelength))
              stop ("The wavelengths of the objects differ.")
            
            x@data <- rbind (x@data, y@data)
            x@log <- .logentry (x, short = "rbind2", long = list (y = as.character (y)))
            
            x
          }
          )
setMethod("rbind2", signature (x = "hyperSpec", y = "missing"), function (x, y) x) 

###-----------------------------------------------------------------------------
###
### cbind & rbind
###  
###  
bind <- function (direction = stop ("direction ('c' or 'r') required"),
                  ..., short = NULL, user = NULL, date = NULL){
  dots <- list (...)
  
  if ((length (dots) == 1) & is.list (dots [[1]]))
    dots <- dots[[1]]
  
  if (length (dots) == 0)
    NULL
  else if (length (dots) == 1){
    validObject (dots[[1]])
    dots[[1]]
  } else {
    if (! is (dots[[1]], "hyperSpec"))
      stop ("bind only works on hyperSpec objects.")
    
    logs <- list() 
    
    for (i in seq_len (length (dots)) [-1]){
      if (! is (dots[[i]], "hyperSpec"))
        stop ("bind only works on hyperSpec objects.")
      validObject (dots[[i]])
      
      dots[[1]] <- switch (direction,
                           c = cbind2 (dots[[1]], dots[[i]]),
                           r = rbind2 (dots[[1]], dots[[i]]),
                           stop ("direction must be either 'c' or 'r' for cbind and rbind, respectively.")
                           )
      
      if (!is.null (short))
        dots[[1]]@log[nrow (dots[[1]]@log), "short.description"] <- paste (short, dots[[1]]@log[nrow (dots[[1]]@log), "short.description"])
      if (!is.null (date))
        dots[[1]]@log[nrow (dots[[1]]@log), "date"] <- date
      if (!is.null (user))
        dots[[1]]@log[nrow (dots[[1]]@log), "user"] <- user
    }
    dots[[1]]
  }
}

cbind.hyperSpec <- function (..., deparse.level) bind ("c", ...)

rbind.hyperSpec <- function (..., deparse.level) bind ("r", ...)
###-----------------------------------------------------------------------------
###
### wl2i
###  
###
wl2i <- function (x, wavelength = stop ("wavelengths are required.")){
  validObject (x)
  
  ## special in formula
  max <- max (x@wavelength) 
  min <- min (x@wavelength) 
  
  `~` <- function (e1, e2){
    if (missing (e2))              # happens with formula ( ~ end)
      stop ("wavelength must be a both-sided formula")
    
    if (    (Re (e1) < min (x@wavelength) && Re (e2) < min (x@wavelength)) ||
        (Re (e1) > max (x@wavelength) && Re (e2) > max (x@wavelength))){
      NULL                       # wavelengths completely outside the wl. range of x 
    } else {
      e1 <- .getindex (x, Re (e1)) + Im (e1)
      e2 <- .getindex (x, Re (e2)) + Im (e2)
      
      if (e1 <= 0 || e2 <= 0|| e1 > length (x@wavelength) || e2 > length (x@wavelength))
        warning ("wl2i: formula yields indices outside the object.")
      
      seq (e1, e2)
    }
  }
  
  .conv.range <- function (range){
    if (is.numeric (range)){
      .getindex (x, range, rule = 1)
    } else 
    eval (range)
  }
  
  if (is.list (wavelength))
    unlist (lapply (wavelength, .conv.range))
  else (.conv.range (wavelength))
}
###-----------------------------------------------------------------------------
###
### i2wl
###  
###

i2wl <- function (x, i){
  validObject (x)
  
  x@wavelength[i]
} 

###-----------------------------------------------------------------------------
###
### .getindex
###  
###  
## does the acual work of looking up the index
## rule = 2 returns first resp. last index for wavelength outside hyperSpec@wavelength.

.getindex <- function (x, wavelength, rule = 2){
  if (length (x@wavelength) == 1)
    1
  else
    round (approx (x = x@wavelength, y = seq_along(x@wavelength),
                   xout = wavelength, rule = rule)$y)
}

###-----------------------------------------------------------------------------
###
###  .na.if.different
###  
###  

.na.if.different <- function (x) {
  if (length (unique (x)) > 1)
    NA
  else
    x[1]
}

###-----------------------------------------------------------------------------
###
###  .apply
###  
###  

.apply <- function (data, MARGIN, FUN, ...){
  
  if (length (data$spc) == 0)
    stop ("empty spectra matrix.") 
  
  spc <- apply (data [, "spc", drop = FALSE], MARGIN, FUN, ...)
  
  if (MARGIN == 1){
    if (is.null (spc))
      spc <- matrix (ncol = 0, nrow = nrow (data))
    else if (is.vector (spc)) 
      dim (spc) <- c(length (spc), 1)
    else if (is.matrix (spc))
      spc <- t (spc)
    
    data$spc <- I(spc)
  } else if (MARGIN == 2){
    if (is.null (spc))
      return (data [0, ])
    if (is.null (dim (spc))) 
      dim (spc) <- c(1, ncol (data$spc))
    
    if (all(dim (spc) == dim (data$spc))){
      data$spc <- spc
    }  else {
      nrow <- nrow (spc)
      
      data <- data[rep (1, nrow), , drop = FALSE] 
      
      cols <- colnames (data)
      cols <- which (cols != "spc")
      if (length (cols) > 0) {
        colvals <- apply (data [,cols,drop = FALSE], 2, .na.if.different)
        data [,cols] <- rep (colvals, each = nrow)
      }
      
      data$spc <- I (spc)
      rownames (data) <- rownames (spc)
    }
  }
  
  data 
}

###-----------------------------------------------------------------------------
###
###  apply
###  
###  

setMethod ("apply", "hyperSpec", function (X, MARGIN, FUN, ...,
                                           label.wl = NULL, label.spc = NULL, new.wavelength = NULL,
                                           short = NULL, long = NULL, user = NULL, date = NULL){
  validObject (X)
  
  if (is.null (short))
    short <- "apply"
  if (is.null (long))
    long <- list (MARGIN = MARGIN, FUN = FUN, ...,
                  call = deparse (sys.call()[])
                  )
  if (all (MARGIN == 1 : 2)){
    X@data$spc <- do.call (FUN, list (X@data$spc, ...)) 
  } else {
    X@data <- .apply(X@data, MARGIN = MARGIN, FUN = FUN, ...)
    
    if (MARGIN == 1) {
      ## no shortcuts here: the validation will fail until the
      ## wavelength axis is adjusted 
      if (!is.null (new.wavelength))
        if (is.numeric (new.wavelength))
          .wl (X) <- new.wavelength
        else {
          dots <- list (...)
          .wl (X) <- dots[[new.wavelength]]
        }
      else if (ncol (X@data$spc) != length (X@wavelength)){ 
        wl <- as.numeric (colnames (X@data$spc))
        if (length (wl) != ncol (X@data$spc) || any (is.na (wl)))
          .wl (X) <- seq_len (ncol (X@data$spc))
        else
          .wl (X) <- wl
      }
      
      if (ncol (X@data$spc) != length (X@wavelength)) 
        X@label$.wavelength <- NULL
      
    }
  }
  
  if (!is.null (label.wl))
    X@label$.wavelength <- label.wl	
  
  if (!is.null (label.spc))
    X@label$spc <- label.spc
  
  X@log <- .logentry(X, short = short, long = long, user = user, date = date)
  
  validObject (X)
  
  X
})


###-----------------------------------------------------------------------------
###
### split - split according to given factor
###  

setMethod ("split", "hyperSpec", function (x, f, drop = TRUE, #...,
                                           short = NULL, user = NULL, date = NULL){
  validObject (x)
  
  hyperlist <- split (seq_len (nrow (x@data)), f, drop) 
  
  log <-  .logentry (x, short = short, long = list (f = f, drop = drop),
                    user = user, date = date)
  
  for (i in seq_len (length (hyperlist))){
    hyperlist[[i]] <- x[hyperlist[[i]],]
    
    hyperlist[[i]]@log <- log
  }
  
  hyperlist
})

###-----------------------------------------------------------------------------
###
### sweep
###  
###

setMethod ("sweep", "hyperSpec", function (x, MARGIN, STATS, FUN = "-",
                                           check.margin = TRUE, ...,
                                           short = NULL, user = NULL, date = NULL){
  validObject (x)
  
  if (is (STATS, "hyperSpec")){
    validObject (STATS) 
    STATS <- STATS@data$spc
  }
  
  x@data$spc <- do.call (sweep, c (list (x = x@data$spc,
                                         MARGIN = MARGIN,
                                         STATS = STATS,
                                         FUN = FUN,
                                         check.margin = check.margin),
                                   ...)
                         )
  
  x@log <- .logentry (x,
                     short = if (!is.null (short)) paste ("sweep (", short, ")", sep = "") else NULL,
                     long = list (MARGIN = MARGIN,
                       FUN = FUN,
                       STATS = STATS,
                       FUN = FUN,
                       check.margin = check.margin,
                       ...),
                     date = date,
                     user = user
                     )  
  
  
  x 
  
})

###-----------------------------------------------------------------------------
###
###  aggregate
###  
###

setMethod ("aggregate", "hyperSpec", function (x,
                                               by = stop ("by is needed: either a grouping vector (e.g. factor) or a list of groups"),
                                               FUN = stop ("FUN is needed."),
                                               ...,
                                               out.rows = NULL, 
                                               append.rows = NULL,
                                               short = NULL, date = NULL, user = NULL){
  
  validObject (x)
  
  long <- list (by = by, FUN = FUN, out.rows = out.rows,
                append.rows = append.rows, ...)
  
  if (!is.list (by))
    by <- split (seq_len (nrow (x)), by, drop = TRUE)
  
  if (is.null (out.rows)){
    dummy <- .apply (data = x@data[by [[1]], , drop = FALSE], MARGIN = 2, FUN = FUN, ...)
    
    out.rows <- nrow (dummy) * length (by)  
  }
  
  data  <- x@data[rep (1, out.rows), , drop = FALSE] # preallocate memory 
  data <- cbind (x@data, .aggregate = NA)
  col.aggregate <- ncol (data)
  
  
  r <- 1 # keeping track of the actually filled rows
  
  for (i in seq (along = by)){
    dummy <- .apply (data = x@data[by [[i]], , drop  = FALSE], MARGIN = 2, FUN = FUN, ...)
    
    prows <- nrow (dummy) - 1
    
    
    if (r + prows > out.rows) {
      if (is.null (append.rows)) 
        append.rows <- max (100, ceiling (1 - (i / length (by)) * out.rows))
      out.rows <- max (append.rows + out.rows, r + prows)
      data  <- rbind (data, data [rep (1, out.rows - nrow (data)), , drop = FALSE])
      warning (paste ("At", i, "of", length (by), "levels: Output data.frame too small. Consider using an appropriate value for out.rows to speed up calculations."))
    }
    
    if (prows >= 0){
      data[r : (r + prows), -col.aggregate] <- dummy
      data[r : (r + prows),  col.aggregate] <- i  
      
      r <- r + prows + 1
    }
  }
  
  x@data <- data[seq_len (r - 1), , drop = FALSE]
  x@data[, col.aggregate] <- factor (x@data[, col.aggregate], levels = seq_len (length (by)))
  
  if (!is.null (names (by)) && !any (is.na (names (by)))) 
    levels (x@data[, col.aggregate]) <- names (by) 
  
  x@log <- .logentry (x, long = long, short = short, date = date, user = user)
  
  x 
  
})

###-----------------------------------------------------------------------------
###
###  decomposition -- decomposes hyperSpec object e.g. into scores and loading objects
###  
###  
decomposition <- function (object, x, wavelength = seq_len (ncol (x)),
                           label.wavelength, label.spc,
                           scores = TRUE, retain.columns = FALSE,
                           short = "", ...){
  validObject (object)
  
  short <- paste (" decomposition:", short, sep ="")
  
  if (is.vector (x))
    if (nrow (object) == length (x))
      dim (x) <- c(length (x), 1)
    else
      dim (x) <- c(1, length (x))
  
  if ((nrow (x) == nrow (object)) & scores){
    
    object@data$spc <- I (as.matrix (x))
    
    .wl (object) <- wavelength
    
    object@label$.wavelength <- label.wavelength
    
  } else if (ncol (x) == nwl (object)){
    spc <- match("spc", colnames(object@data))
    cols <- apply(object@data[, -spc, drop = FALSE], 2,
                  .na.if.different)
    object@data[1, -spc] <- cols
    
    if (!retain.columns) {
      cols <- which(is.na(object@data[1, -spc]))
      cols[cols > spc] <- cols [cols > spc] + 1
      object@data <- object@data[, -cols, drop = FALSE]
    }
    
    object@data <- object@data[rep(1, nrow(x)), , drop = FALSE]
    object@data$spc <- I(as.matrix(x))
  } else {
    stop ("Either rows or columns of x and object must correspond")
  }
  
  object@label$spc <- label.spc
  object@log <- .logentry (object, short = short, ...)
  
  validObject (object)
  object
}

### ****************************************************************************
###
###  plotting functions
###  
### ****************************************************************************

###-----------------------------------------------------------------------------
###
###  plotmap - plot map 
###

plotmap <- function (object,                                             
                     x = "x",
                     y = "y",
                     func = mean,
                     cond = NULL,
                     z = NULL,
                     do.print = FALSE,
                     print.args = NULL,
                     trellis.args = NULL,
                     ...
                     ) {
  validObject (object)
  
  ix <- pmatch (x, colnames (object@data))
  if (is.null (ix))
    stop (paste ("hyperSpec object has no column ", x))
  
  iy <- pmatch (y, colnames (object@data))
  if (is.null (iy))
    stop (paste ("hyperSpec object has no column ", y)) 
  
  if (!is.null (z)){
    if (is.numeric (z)){
      z <- rep (z, length.out = nrow (object))
    } else if (is.character (z)){
      if (is.na (match (z, colnames (object@data))))
        stop ("z did not evaluate to a column in object@data.")
      z <- object@data[, z]
    } 
  } else {
    z <- apply (object [[]], 1, func, ...)
    if (length (z) != nrow (object))
      warning ("func did not yield one value per spectrum.")
  }

  if (! is.null (cond)){
    if (is.character (cond) && length (cond) == 1) {
      if (is.na (match (cond, colnames (object@data))))
        stop ("cond did not evaluate to a column in object@data.")
      cond <- object@data[, cond]
    }
    dots <- list (x = formula (z ~ x * y | cond))
    if (is.null (trellis.args$data))
      dots$data <- data.frame (x = object[[, ix]] , y = object[[, iy]], z = as.numeric(z), cond = cond)
  } else { 
    dots <- list (x = formula (z ~ x * y))
    if (is.null (trellis.args$data))
      dots$data <- data.frame (x = object[[, ix]] , y = object[[, iy]], z = as.numeric(z))
  }
    
  n <- length (unique (zapsmall (as.numeric (z))))
  if (is.null (trellis.args$col.regions)){
    if (is.factor (z))
      trellis.args$col.regions <- matlab.palette (nlevels (z))
    else
      trellis.args$col.regions <- matlab.palette ()
  } else if (is.factor (z))
    trellis.args$col.regions <- rep (trellis.args$col.regions, length.out = nlevels (z))
  
  if (is.null (trellis.args$at)){
    if (is.factor (z)) {
      trellis.args$at <- seq_len (nlevels (z) + 1) - 0.5
    } else if (n == 1) {
      trellis.args$at <- range (z)[1] * c (.99, 1.01)
      if (all (trellis.args$at == 0))
        trellis.args$at <- c(-1, 1)
    } else {
      trellis.args$at = seq (min (z), max (z), length.out = length (trellis.args$col.regions) + 1)
    }
  }
  
  if (is.null (trellis.args$aspect))
    trellis.args$aspect = "iso"
  
  if (is.null (trellis.args$xlab)){
    trellis.args$xlab <- object@label[[names(object@data)[ix]]] 
    if (is.null (trellis.args$xlab))
      trellis.args$xlab <- x  
  }
  
  if (is.null (trellis.args$ylab)){
    trellis.args$ylab = object@label[[names(object@data)[iy]]] 
    if (is.null (trellis.args$ylab))
      trellis.args$ylab <- y
  }

  if (is.null (trellis.args$panel)){
    trellis.args$panel <- function (x, y, z, subscripts, ..., panel.bg = NA) {
      dummy <- index.grid (x[subscripts], y[subscripts], z [subscripts])
      panel.fill (col = panel.bg) 
      panel.levelplot (dummy$x, dummy$y, dummy$z, subscripts = TRUE, ...)     
    } 
  }

  lattice <- do.call(levelplot, c(dots, trellis.args))
  dots$x <- lattice
  if (do.print)
    do.call (print, c(print.args, dots))
  
  lattice
}

###-----------------------------------------------------------------------------
###
### index.grid
###  
###  
index.grid <- function (x, y, z){
  if (is (x, "hyperSpec")){
    validObject
    y <- x$y
    x <- x$x
  } else if ((is.matrix (x) || is.data.frame (x)) && ncol (x) == 2) {
    y <- x [,2]
    x <- x [,1]
  } else if (missing (y))
    stop ("y can only be omitted if x is a hyperSpec object or a matrix or data.frame with 2 columns.") 

  if (is.data.frame (x)) 
    x <- x [[1]]
  if (is.data.frame (y)) 
    y <- y [[1]]
  
  if (length (x) != length (y))
    stop ("x and y need to have the same length") 
  
  order <- order (cbind (x, y))
  
  x <- as.ordered (x)
  y <- as.ordered (y)
  levx <- as.numeric (levels (x))
  levy <- as.numeric (levels (y))
  x <- as.numeric (x)
  y <- as.numeric (y) 
  
  grid <- matrix (nrow = length (levx),
                  ncol = length (levy),
                  dimnames = list (x = levx, y = levy))
  
  for (i in seq (along = x)){
    grid [x[i], y[i]] <- i
  }
  
  if (!missing (z)){
    z <- z [grid]
    dim(z) <- dim (grid)
    dimnames (z) <- dimnames (grid) 
  } else {
    z <- NULL
  }
  
  x <- levx[row(grid)]
  dim (x) <- dim (grid)  
  
  y <- levy[col(grid)]
  dim (y) <- dim (grid) 
  
  list (grid = grid, x = x, y = y, z = z)
} 

###-----------------------------------------------------------------------------
###
### map.identify
###  
###  

map.identify <- function (object, x = "x", y = "y", ...){
  validObject (object)
  
  ix <- pmatch (x, colnames (object@data))
  if (is.null (ix))
    stop (paste ("hyperSpec object has no column ", x))
  
  iy <- pmatch (y, colnames (object@data))
  if (is.null (iy))
    stop (paste ("hyperSpec object has no column ", y)) 

  lattice <- plotmap(object, x, y, ...) 
  
  print (lattice)
  trellis.focus ()
#  panel.identify (subscripts = mesh)
  panel.identify (x = unlist (object[[, ix]]), y = unlist (object[[, iy]])) 
}

###-----------------------------------------------------------------------------
###
### spc.identify
###  
###  

spc.identify <- function (x, y = NULL, wavelengths = NULL, ispc = NULL, ...){
  if (is.list (x)) {
    if (is.null (wavelengths)) 
      wavelengths <- x$wavelengths
    if (is.null (y)) 
      y <- x$y
    x <- x$x
  }
  
  if ((length (x) != length (y)) | (length (x) != length (wavelengths)))
    stop ("x, y, and wavelength need to have the same length.") 
  
  if (is.null (ispc)) 
    ispc <- row (y)
  else
    ispc <- ispc[row(y)] 
  
  i <- identify (x, y,
                 labels = paste (ispc, format (wavelengths, digits = 4),
                   sep = ", "),
                 ...
                 )
  
  data.frame (i = ispc [i], wavelengths = wavelengths [i]) 
}

###-----------------------------------------------------------------------------
###
###  plotc - plot timeseries, concentration, ... 
###  
plotc <- function (object, use.c = "c", func = sum, ...,  
                   z = NULL, zlab = NULL, add = FALSE,
                   plot.args = list()) {
  validObject (object)
  
  ic <- pmatch (use.c, colnames (object@data))
  if (is.null (ic))
    stop (paste ("hyperSpec object has no column ", use.c))
  
  if (!is.null (z)){
    if (is.numeric (z)){
      z <- rep (z, length.out = nrow (object))
      if (is.null (zlab))
        zlab = "z"
    } else if (is.character (z)){
      if (is.na (match (z, colnames (object@data))))
        stop ("z did not evaluate to a column in object@data.")
      if (is.null (zlab))
        zlab <- object@label[[z]]
      if (is.null (zlab))
        zlab <- z
      z <- object@data[, z]
    } 
  } else {
    z <- apply (object [[]], 1, func, ...)
    
    if (is.null (zlab)){
      zlab <- as.expression (substitute (func))
      if (nwl (object) == 1 & as.character(zlab) == "sum")
        zlab <- object@label$spc
      else
        zlab <-  paste (zlab, object@label$spc)
    }
  }
  
  plot.args <- c(list (x = unlist (object[[, ic]]),
                       y  = as.numeric (z)),
                 plot.args)
  
  if (is.null (plot.args$xlab)){
    plot.args$xlab <- object@label[[names(object@data)[ic]]] 
    if (is.null (plot.args$xlab))
      plot.args$xlab <- use.c  
  }
  
  if (is.null (plot.args$ylab)){
    plot.args$ylab <- zlab
    if (is.null (plot.args$ylab))
      plot.args$ylab <- z
  }
  
  if (is.null (plot.args$pch))
    plot.args$pch = 20
  
  if (is.null(plot.args$type)) 
    plot.args$type = "p"
  
  if (add)
    do.call(lines, plot.args)
  else
    do.call(plot, plot.args)
}

###-----------------------------------------------------------------------------
###
###  plotspc - Plots spectra of hyperSpec object
###  
###  convenient plot interface for plotting spectra
###  

plotspc <- function  (object,
                      ## what wavelengths to plot
                      wl.range = NULL, 
                      wl.index = FALSE,
                      wl.reverse = FALSE,
                      ## what spectra to plot 
                      spc.nmax = 50,
                      func = NULL,
                      func.args = list (),
                      stacked = NULL,
                      ## plot / lines
                      add = FALSE,
                      bty = "l",
                      col = "black",
                      plot.args = list() ,
                      ## axes
                      xoffset = 0,
                      yoffset = 0,
                      nxticks = 10,
                      axis.args = list () ,
                      ## parameters for filled regions
                      fill = NULL,
                      fill.col = NULL,
                      border = NA, 
                      title.args = list (),
                      polygon.args = list (),
                      lines.args = list (),
                      zeroline =  list (lty = 2, col = col)#if (all (yoffset == 0)) list (lty = 2) else NULL
                      ){
  force (zeroline) # otherwise stacking messes up colors
  
  validObject (object)
  
  if (nrow (object) == 0)
    stop ("No spectra.")
  
  ## prepare wavelengths
  if (is.null (wl.range)){
    wl.range <- list (seq (along = object@wavelength));
  } else {
    if (!wl.index){
      for (i in seq (along = wl.range))
        wl.range[[i]] <- wl2i (object, wl.range[[i]])
    }
    
    if (!is.list (wl.range))
      wl.range <- list (wl.range)
    
    for (i in seq (along = wl.range))
      if (!is.numeric (wl.range[[i]]) && !is (wl.range [[i]], "formula"))
        stop ("wavelength ranges need to be numeric or formulas.")
    
    for (i in seq (along = wl.range))
      wl.range[[i]] <- unique (wl.range[[i]][!is.na (wl.range[[i]])])
  }
  
  ## xoffset
  if (length (xoffset) == length (wl.range) - 1)
    xoffset = c (0, xoffset) 
  else if (length (xoffset) == 1)
    xoffset = rep (xoffset, times = length (wl.range))
  if (!is.numeric(xoffset) || (length (xoffset) != length (wl.range)))
    stop ("xoffset must be a numeric  vector of the same length as the list with wavenumber ranges.")
  xoffset <- cumsum (xoffset) 
  
  ## for indexing wavelength.range is needed unlisted
  u.wl.range <- unlist (wl.range)
  
  ## wavelengths are the numbers to print at the x axis
  wavelengths <- relist (object@wavelength [u.wl.range], wl.range)
  
  ## x are the actual x coordinates
  x <- wavelengths
  for (i in seq_along(wl.range))
    x [[i]] <- x [[i]] - xoffset[i]
  
  ## indices into columns of spectra matrix spc 
  ispc <- relist (seq_len (length (u.wl.range)), wl.range)
  
  rm (wl.range)
  spc <- object[[,, u.wl.range, drop = FALSE, wl.index = TRUE]]
  rm (u.wl.range)
  
  
  ## apply function func to spc
  if (!is.null (func)){
    if (!is.function (func))
      stop ("func needs to be a function.");
    
    apply.args <- c (list (X = spc, MARGIN = 2, FUN = func), func.args)
    spc <- matrix (do.call (apply, apply.args),  #apply (spc, 2, func),
                   ncol = ncol (spc)
                   )
    if (nrow (spc) == 0)
      stop ("No spectra after func was applied.")
  }
  
  
  
  if (length (yoffset) != nrow (spc)){
    if (length (yoffset) == 1)
      yoffset <- rep (yoffset, nrow (spc))
    else
      stop ("yoffset must be single number.")
  }
  
  if (nrow (spc) > spc.nmax){
    warning (paste ("Number of spectra exceeds spc.nmax. Only the first",
                    spc.nmax, "are plotted."))
    spc <- spc [seq_len (spc.nmax), , drop = FALSE]
    yoffset <- yoffset [seq_len (spc.nmax)]
  }
  
  ## stacked plot
  if (!is.null (stacked)){
    stacked <- stacked.offsets (object, stacked, spc)
    yoffset <- stacked$offsets [stacked$groups]
  }
  
  spc <- sweep (spc, 1, yoffset, "+")
  
  if (! add){
    ## Plot area
    plot.args$x <- unlist (x)
    plot.args$y <- spc[1,,drop=FALSE]
    plot.args$type <- "n"
    plot.args$bty <- bty
    plot.args$xaxt <- "n"
    plot.args$yaxt <- "n"
    plot.args$xlab <- NA     # title is called later
    plot.args$ylab <- NA
    
    if (is.null (plot.args$xlim))
      plot.args$xlim <- range (unlist (x), na.rm = TRUE)
    
    if (is.null (plot.args$ylim))
      plot.args$ylim <- range (spc, na.rm = TRUE)
    
    ## reverse x axis ?
    if (wl.reverse)
      plot.args$xlim <- rev(plot.args$xlim)
    
    do.call (plot, plot.args)
    
    ## reversed x axis ? => would lead to trouble with tick positions
    if (diff (plot.args$xlim) < 0)
      plot.args$xlim <- rev(plot.args$xlim)
    
    
    ## Axes
    ## x-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u", "]") ){
      if (is.null (axis.args$x))
        axis.args$x <- list ()
      if (is.null (axis.args$x$side))
        axis.args$x$side <- 1
      
      ## Tick mark positions
      if (is.null (axis.args$x$at)){
        if (all (xoffset == 0)){
          axis.args$x$at <- pretty (plot.args$xlim +
                                    diff(plot.args$xlim) * c(-0.04, 0.04),
                                    nxticks)
        } else {
          axis.args$x$at <- list ()
          
          part <- apply (sapply (wavelengths, range), 2, diff) /
            diff (plot.args$xlim) 
          
          for (i in seq_along (x))  
            axis.args$x$at [[i]] <- pretty (wavelengths[[i]],
                                            part [i] * nxticks + 1)
        }          
      }
      if (!is.list (axis.args$x$at))
        axis.args$x$at <- rep (list (axis.args$x$at), length (x))
      
      ## calculate cut mark positions and which ticks are to be displayed
      cutmarks <- numeric (length (x) - 1)
      
      for (i in seq_along (axis.args$x$at)[-1]){
        a <- max (x [[i - 1]])
        b <- min (x [[i    ]])
        delta <- b - a
        cutmarks [i - 1] <- a + delta / 2
        
        a <- a + xoffset [i] + delta / 4
        b <- b + xoffset [i] - delta / 4
        
        axis.args$x$at [[i - 1]] <- axis.args$x$at [[i - 1]][axis.args$x$at [[i - 1]] < a]
        axis.args$x$at [[i    ]] <- axis.args$x$at [[i    ]][axis.args$x$at [[i    ]] > b]
      }
      
      ## Tick mark labels      
      if (is.null (axis.args$x$labels)){
        axis.args$x$labels <- (axis.args$x$at)
        for (i in seq_along (axis.args$x$at))
          axis.args$x$at [[i]] <- axis.args$x$at [[i]] - xoffset [i]
      } 
      
      axis.args$x$at <- unlist (axis.args$x$at)
      axis.args$x$labels <- unlist (axis.args$x$labels) 
      
      do.call (axis, axis.args$x)
      
      ## plot cut marks for x axis
      for (i in seq_along (cutmarks))
        if (xoffset[i + 1] != 0)
          mtext("//", at = cutmarks [i], side = 1, padj = -1, adj = 0.5)
    }
    
    ## y-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u")){
      if (is.null (axis.args$y))
        axis.args$y <- list ()
      if (is.null (axis.args$y$side))
        axis.args$y$side <- 2
      if (is.null (axis.args$y$at) & !is.null (stacked)){
        axis.args$y$at <- apply (spc[!duplicated (stacked$groups),], 1, min) #apply (spc, 1, min)
        axis.args$y$labels <- stacked$levels #seq_len (nrow (spc))
      }
      
      do.call (axis, axis.args$y)
    }
    
    
    ## Title
    if (is.null (title.args$xlab))
      title.args$xlab <- list ()
    if (!is.list (title.args$xlab)) 
      title.args$xlab <- list (xlab = title.args$xlab)
    else
      title.args$xlab$xlab <- I(object@label$.wavelength)
    if (names (title.args$xlab) [1] == "")
      names (title.args$xlab) [1] <- "xlab"
    
    if (is.null (title.args$xlab$line))
      title.args$xlab$line <- 2.5
    
    if (is.null (title.args$ylab))
      title.args$ylab <- list () 
    if (!is.list (title.args$ylab)) 
      title.args$ylab <- list (ylab = title.args$ylab)
    else
      title.args$ylab$ylab <- I(object@label$spc)
    if (names (title.args$ylab) [1] == "")
      names (title.args$ylab) [1] <- "ylab"   
    
    titles <- pmatch (c("main", "sub", "xlab", "ylab"), names (title.args))
    other <- !(seq (along = title.args) %in% titles) 
    
    for (i in titles)
      do.call (title, c(title.args[[i]], title.args[other]))
  }
  
  col <- rep (col, each = ceiling (nrow (spc) / length (col)), length.out = nrow (spc))
  
  ## start loop over wavelength ranges
  for (i in seq_along (x)){
    if (!is.null (fill)){
      if (is.character (fill))
        fill <- unlist (object [[, fill]])
      else if (isTRUE (fill)){
        fill <- seq_len (nrow (spc)) / 2
        if (nrow (spc) %% 2 == 1) # odd number of spectra
          fill <- c (fill, NA, rev (fill))
        else
          fill <- c (fill, rev (fill))
      } 
      if (is.factor (fill))
        fill <- as.numeric (fill)
      else if (!is.numeric (fill))
        stop ("fill must be either TRUE, the name of the extra data column to use for grouping, a factor or a numeric.")
      
      groups = unique (fill)
      groups = groups [!is.na (groups)]
      
      if (is.null(polygon.args$x))
        polygon.args <- c(list(x = NULL, y = NULL), polygon.args)
      
      if (is.null (fill.col)){
        fill.col <- character (length (groups))
        for (j in seq_along (groups)){
          dummy <- which (fill == groups [j])
          fill.col [j] <- rgb(t(col2rgb(col[dummy[1]]) / 255) / 3 + 2/3)
        }
      } else {				
        fill.col <- rep (fill.col, length.out = length (groups))
      }
      
      border <- rep (border, length.out = length (groups)) 
      
      polygon.args$x <- c (x  [[i]]                  , rev (x   [[i]]         ))
      for (j in seq_along (groups)){
        dummy <- which (fill == groups [j])
        polygon.args$y <- c (spc[head(dummy, 1), ispc[[i]]], rev (spc [tail (dummy, 1), ispc[[i]]]))
        polygon.args$col = fill.col [groups [j]]
        polygon.args$border <- border [groups [j]]
        
        do.call (polygon, polygon.args)
      }
    }
    
    if (is.null(lines.args$x)) 
      lines.args <- c(list (x = NULL, y = NULL), lines.args)
    
    if (is.null (lines.args$type))
      lines.args$type <- "l"
    
    for (j in seq_len (nrow (spc))){
      lines.args$x <- x[[i]]
      lines.args$y <- spc [j, ispc[[i]]]
      lines.args$col <- col [j]
      
      do.call (lines, lines.args) 
    }
  }
  
  
  if (! is.null (zeroline)){
    zeroline <- c (list (h = unique (yoffset)), zeroline)
    
    do.call (abline, zeroline)
  }
  
  invisible (list (x = rep (unlist (x), each = nrow (spc)) ,
                   y = spc,
                   wavelengths = rep (unlist (wavelengths), each = nrow (spc))
                   ))
}

###-----------------------------------------------------------------------------
###
###  stacked.offsets
###  

stacked.offsets <- function (x, stacked = TRUE, .spc = NULL){
  lvl <- NULL
  
  if (is.character (stacked))
    stacked <- unlist (x [[, stacked]])
  else if (isTRUE (stacked))
    stacked <- seq (nrow (x@data))
  
  if (is.factor (stacked)) {
    lvl <- levels (stacked)
    stacked <- as.numeric (stacked)
  } else if (!is.numeric (stacked))
    stop ("stacked must be either TRUE, the name of the extra data column to use for grouping, a factor or a numeric.")
  
  if (is.null (.spc))
    .spc <- x@data$spc
  
  ## using ave would be easier, but it splits the data possibly leading to huge lists.
  groups <- unique (as.numeric (stacked))  
  offset <- matrix (nrow = 2, ncol = length (groups))
  for (i in seq_along (groups))
    offset[, i] <- range (.spc [stacked == groups[i], ], na.rm = TRUE)
  
  
  offset [2,] <- offset[2,] - offset [1,]
  offset <- c(-offset[1,], 0) + c(0, cumsum (offset[2,]))
  list (offsets = offset [seq (length (groups))], 
        groups = stacked, 
        levels = if (is.null (lvl)) stacked else lvl
        )
}

###-----------------------------------------------------------------------------
###
###  plot methods
###  


### use plotspc as default plot function
setMethod ("plot",
           signature (x = "hyperSpec", y = "missing"),
           function (x, y, ...) plotspc (x, ...)
           )

### allow choice of spectral or map plot by second argument
setMethod ("plot",
           ##    'spc'        ... spectra
           ##    'map'        ... map
           ##    'c'          ... concentration: plotc
           ##    'ts'         ... time series: plotc
           ##    'depth'      ... concentration or time series
           ##    'spcmeansd'  ... mean spectrum +- 1 standard deviation
           ##    'spcprctile' ... median spectrum , 16th and 84th percentile
           ##    'spcprctl5'  ... spcprctile plus 5th and 95th percentile
           
           signature (x = "hyperSpec", y = "character"),
           function (x, y, ...){
             dots <- list(...)          # to allow optional argument checks
             
             if (missing (y)){
               stop ("shouldnt be")
               y = "spc"
             }
             
             switch (tolower (y),
                     spc = {
                       plotspc (x, ...)
                     },
                     spcmeansd = {
                       dots$fill <- c (1, NA, 1)
                       if (is.null (dots$func.args))
                         dots$func.args <- list (na.rm = TRUE)
                       else if (is.null (dots$func.args$na.rm))
                         dots$func.args$na.rm <- TRUE
                       dots$object <- x
                       dots$func <- mean_pm_sd
                       do.call (plotspc, dots) 
                     },
                     spcprctile = {
                       dots$fill <- c (1, NA, 1)
                       if (is.null (dots$func.args))
                         dots$func.args <- list ()
                       if (is.null (dots$func.args$na.rm))
                         dots$func.args$na.rm <- TRUE
                       if (is.null (dots$func.args$probs))
                         dots$func.args$probs = c (0.16, 0.5, 0.84)
                       dots$object <- x
                       dots$func <- quantile
                       do.call (plotspc, dots) 
                     },
                     spcprctl5 = {
                       if (is.null (dots$fill.col)) {
                         dots$fill.col <- c(rgb(t(col2rgb("black")/255)/2 + c (1, 1, 1) * .75), 
                                            rgb(t(col2rgb("black")/255)/2 + c (1, 1, 1) * .50))
                       }
                       dots$fill <-  c(1, 2, 3, 2, 1)
                       if (is.null (dots$func.args))
                         dots$func.args <- list ()
                       if (is.null (dots$func.args$na.rm))
                         dots$func.args$na.rm <- TRUE
                       if (is.null (dots$func.args$probs))
                         dots$func.args$probs = c (0.05, 0.16, 0.5, 0.84, 0.95)
                       dots$object <- x
                       dots$func <- quantile
                       do.call (plotspc, dots) 
                     },
                     map = plotmap (x, ...),
                     c = plotc (x, ...),
                     ts = {
                       dots <- list (object = x, ...)
                       if (is.null (dots$use.c))
                         dots$use.c = "t"
                       do.call (plotc, dots)
                     },
                     depth = {
                       dots <- list (object = x, ...) 
                       if (is.null (dots$use.c))
                         dots$use.c = "z"
                       do.call (plotc, dots)
                     },
                     stop (paste ("y = ", y, "unknown.", collapse = " ")) 
                     )
           }
           )


### ****************************************************************************
###
###  read & write hyperSpec objects
###  
### ****************************************************************************

###-----------------------------------------------------------------------------
###
###  read.txt.long: was .Renishaw - import Raman measurements from Renishaw .txt file
###  
###  Renishaw .wxd files are converted to .txt ASCII files by their batch converter. 
###  Format:
###  (y x) wl int
###  

read.txt.long <- function (file = stop ("filename is required"),
                           cols = list (
                             wavelength = expression (lambda / nm),
                             spc = "I / a.u."),
                           header = TRUE,
                           ...){
  txtfile <- read.table (file = file, header = header, ...)
  
  if (header){
    cln <- match (colnames (txtfile), names (cols))
    cln <- cols[cln]
    names (cln) <- colnames (txtfile)
    cols <- cln
    rm (cln)
  } else {
    if (ncol (txtfile) != length (cols)){
      warning (paste ("cols does not correspond to the columns in", file,
                      ". Guessing remaining columns."))
      cols <- c (character (ncol (txtfile) - 2), cols)
    }
  }
  
  
  if (is.na (match ("spc", names (cols))))
    stop ("cols$spc must exist.")
  
  wavelength <- match ("wavelength", names (cols))
  if (is.na (wavelength))
    stop ("cols$wavelength must exist.")
  else
    names (cols) [wavelength] <- ".wavelength"
  
  colnames (txtfile) <- names (cols)
  
                                        # wavelength axis
  wavelength <- rep (TRUE,  nrow (txtfile))				
  for (i in which (! colnames (txtfile) %in% c(".wavelength", "spc")))
    wavelength [wavelength] <- txtfile [wavelength, i] == txtfile [1, i]
  
  wavelength <- txtfile$.wavelength [wavelength]
  
  spc <- as.matrix (unstack (txtfile, form = spc ~ .wavelength))
  if ((nrow (spc)  == length (wavelength)) & (ncol (spc) != length (wavelength)))
    spc <- t (spc) 
  
  colnames (spc) <- levels (txtfile$.wavelength)
  
  txtfile <- txtfile [txtfile$.wavelength == txtfile$.wavelength[1], ]
  txtfile$.wavelength <- NULL
  txtfile$spc <- I (spc)
  
  new ("hyperSpec",
       wavelength = wavelength,
       data = txtfile,
       label = cols,
       log = list (
         short = "read.txt.long",
         long = list (file = file, cols = I (cols), ...)
         )
       ) 
}

###-----------------------------------------------------------------------------
###
###  read.txt.wide
###  
###  Format:
###  x y ... int (wl1)  int (wl2) ... int (wl p) z ...
###  
read.txt.wide <- function (file = stop ("filename is required"),
                           cols = list (
                             spc = "I / a.u.",
                             .wavelength = expression (lambda / nm)),
                           check.names = FALSE, 
                           ...){
  txtfile <- read.table (file = file, ..., check.names = FALSE)
  
  .wavelength <- match (".wavelength", names (cols))
  if (is.na (.wavelength))
    cols <- c (cols, .wavelength = expression (lambda / nm))
  else  
    if (.wavelength != length (cols)) ## .wavelength should be at the end of cols
      cols <- cols [c (seq_along (cols)[-.wavelength], .wavelength)]
  
                                        # columns containing the spectra
  spc <- match ("spc", names (cols))
  if (is.na (spc))
    stop ("cols$spc must exist.")
  
  spc <- 0 : (ncol (txtfile) - length (cols) + 1) + spc
  
  spc.data <- as.matrix (txtfile[, spc])
  txtfile$spc <- I (spc.data)	
  txtfile <- txtfile [, -spc, drop = FALSE]
  
  
  new ("hyperSpec",
       data = txtfile,
       label = cols,
       log = list (
         short = "read.txt.long",
         long = list (file = file, cols = I (cols), ...)
         )
       ) 
}

###-----------------------------------------------------------------------------
###
###  read.ENVI
###  
###  read ENVI files, missing header files may be replaced by list in parameter 
###  header
###
###  
read.ENVI <- function (file = stop ("read.ENVI: file name needed"), header = NULL,
                       x = 0 : 1, y = x,
                       wavelength = NULL, label = NULL, log = NULL) {
  cat ('
This function could not be tested extensively up to now.
It was tested with ENVI data written by Bruker and Nicolet spectrometers.
If you have other ENVI files, please contact the maintainer: e-mail to Claudia Beleites <cbeleites@units.it>.

')
  if (!file.exists(file)) 
    stop("read.ENVI: Could not open binary file: ", file)

  recognized.keywords <- c("samples", "lines", "bands", "data type", "header offset", 
              "interleave", "byte order", "wavelength")

  if (!is.list (header)) {
    if (is.null (header)) {
      header <- paste (dirname (file), sub ("[.][^.]+$", ".*", basename (file)), sep = "/")
      dummy <- Sys.glob (header)
      header <- dummy [! grepl (file, dummy)]

      if (length (header) != 1)
        stop ("read.ENVI: cannot guess header file name")
      else
        warning ("read.ENVI: guessing header file name (", header, ")")
    }
    
    if (!file.exists(header)) 
      stop("read.ENVI: Could not open header file: ", header)
    header <- readLines (header)

    ## check ENVI at beginning of file
    if (! grepl ("ENVI", header [1]))
      stop ("read.ENVI: not an ENVI header (ENVI keyword missing)")
    header <- header [-1]

    ## processing is a bit tricky: Bruker wraps multiline values in curly braces, Nicolet uses curly braced inside one line
    ## remove { ... } in one line
    header <- gsub ("\\{([^}]*)\\}", "\\1", header)

    ## remove multiline curly braces
    l <- grep ("\\{", header)
    r <- grep ("\\}", header)

    if (length (l) != length (r))
      stop ("read.ENVI: error matching curly braces in header (differing numbers).")

    if (any (r <= l))
      stop ("read.ENVI: mismatch of curly braces in header.")
    
    header [l] <- sub ("\\{", "", header [l])
    header [r] <- sub ("\\}", "", header [r])

    for (i in rev (seq_along (l))) {
      header <- c (header [seq_len (l [i] - 1)],
                   paste (header [l [i] : r [i]], collapse = " "),
                   header [- seq_len (r [i])])
    }

    ## now ready to separate into key = value pairs
    tcon <- textConnection (header)
    header <- read.table (tcon, sep = "=", strip.white = TRUE, colClasses = rep ("character", 2))
    close (tcon)
    
    header [, 1] <- tolower (header [, 1])

    dummy <- as.list (header [, 2])
    names (dummy) <- header [, 1]
    header <- dummy
  }

  ## check for known and unknown key-value pairs
  unknown <- header [! names (header) %in% recognized.keywords]

  header <- header [names (header) %in% recognized.keywords]

  if (! is.null (header$wavelength)) {
    header$wavelength <- as.numeric (unlist (strsplit (header$wavelength, "[,;[:blank:]]+")))

    if (! any (is.na (header$wavelength))) { 
      if (! is.null (wavelength))
        warning ("read.ENVI: wavelength is overwritten by header field wavelength.")
      wavelength <- header$wavelength
    } 
  }

  dummy <- names (header) %in% c("samples", "lines", "bands", "data type", "header offset")
  header [dummy] <- lapply (header [dummy], as.numeric)

  if (any (is.null (header [c("samples", "lines", "bands", "data type")]) ||
           is.na   (header [c("samples", "lines", "bands", "data type")]) ))
    stop("read.ENVI: Error in header file (required entry missing or incorrect)\n header: ",
         paste (names (header), " = ", header, collapse = ", "))

  if (header$samples <= 0)
    stop("read.ENVI: Error in header file: incorrect data size (", header$samples, ")")
  if (header$lines <= 0)
    stop("read.ENVI: Error in header file: incorrect data size (", header$lines, ")")
  if (header$bands <= 0)
    stop("read.ENVI: Error in header file: incorrect data size (", header$bands, ")")
  
  if (!(header$`data type` %in% c(1 : 5, 9, 12))) 
    stop("read.ENVI: Error in header file: data type incorrect or unsupported (", header$`data type`,")")

  if (is.null (header$`byte order`)){
    header$`byte order` <- .Platform$endian
    warning ("read.ENVI: byte order not given or incorrect. Guessing '", .Platform$endian, "'")
  }
  if (! header$`byte order` %in% c ("big", "little", "swap")) {
    header$`byte order` <- as.numeric (header$`byte order`)
    if (! header$`byte order` %in% 0 : 1) {
      header$`byte order` <- .Platform$endian
      warning ("read.ENVI: byte order not given or incorrect. Guessing '", .Platform$endian, "'")
    } else if (header$`byte order` == 0)
      header$`byte order` <- "little"
    else 
      header$`byte order` <- "big"
  }

  n <- header$samples * header$lines * header$bands

  f <- file (file, "rb")
  if (! is.null (header$`header offset`)) 
    readBin(f, raw(), n = header$`header offset`)
  
  switch(header$`data type`,
         spc <- readBin(f, integer(), n = n, size =  1, signed = FALSE),
         spc <- readBin(f, integer(), n = n, size =  2, endian = header$`byte order`),
         spc <- readBin(f, integer(), n = n, size =  4, endian = header$`byte order`),
         spc <- readBin(f, double(),  n = n, size =  4, endian = header$`byte order`),
         spc <- readBin(f, double(),  n = n, size =  8, endian = header$`byte order`),
         , # 6 unused
         , # 7 unused
         , # 8 unused
         spc <- readBin(f, complex(), n = n, size = 16, endian = header$`byte order`),
         , # 10 unused
         , # 11 unused
         spc <- readBin(f, integer(), n = n, size =  2, endian = header$`byte order`, signed = FALSE)
         )
  
  close(f)

  if (is.null (header$interleave))
    header$interleave <- "bsq"    # de
  
  switch (tolower (header$interleave),
          bil = {dim (spc) <- c(header$samples, header$bands, header$lines); spc <- aperm(spc, c(3, 1, 2))},
          bip = {dim (spc) <- c(header$bands, header$samples, header$lines); spc <- aperm(spc, c(3, 2, 1))},
          bsq = {dim (spc) <- c(header$samples, header$lines, header$bands); spc <- aperm(spc, c(2, 1, 3))},
          stop ("read.ENVI: unknown interleave (", header$interleave, ", should be one of 'bsq', 'bil', 'bip')")
          )

  dim (spc) <- c (header$samples * header$lines, header$bands)

  ## y must be first as it defaults to x

  y <- rep (seq (0, header$lines   - 1),        header$samples) * y [2] + y [1]
  x <- rep (seq (0, header$samples - 1), each = header$lines)   * x [2] + x [1]

  if (length (unknown) > 0)
    data <- data.frame (x = x, y = y, unknown)
  else
    data <- data.frame (x = x, y = y)
  
  new ("hyperSpec", spc = spc, data = data, 
       wavelength = wavelength, label = label, log = log)
}

###-----------------------------------------------------------------------------
###
### scan.txt.Renishaw - import Renishaw Raman ASCII files
###  
### In general this is a long ASCII format. But this function offers chunk-wise
### reading to save memory.
### 
scan.txt.Renishaw <- function (file = stop ("filename is required"), data = "xyspc", 
                               nlines = 0, nspc = NULL, ...){
  cols <- switch (data,
                  spc = NULL,   
                  xyspc = list (y = expression ("/" (y, mu * m)), 
                    x = expression ("/" (x, mu * m))), 
                  zspc = ,
                  depth = list (z = expression ("/" (z, mu * m))),
                  ts = 	list (t = "t / s"),
                  stop ("unknown format for Renishaw .txt files.")
                  )
  cols <- c  (cols, list (.wavelength = expression (Delta * tilde(nu) / cm^-1) ,
                          spc = "I / a.u."))	
  
  first <- scan(file, nlines = 1, quiet = TRUE)
  ncol <- length (first)
  
  if (ncol == 0)
    return (new ("hyperSpec"))
  
  if (ncol != length (cols))
    stop (paste ("File has", ncol, "columns, while 'cols' gives", length (cols)))
  
  file <- file (file, "r")
  on.exit(close(file))
  
  fbuf <- matrix (scan (file, quiet = TRUE, nlines = nlines), ncol = ncol, byrow = TRUE)
  
                                        # wavelength axis
  wl <- rep (TRUE,  nrow (fbuf))				
  for (i in seq_len (ncol (fbuf) - 2))
    wl [wl] <- fbuf [wl, i] == fbuf [1, i]
  
  wl <- fbuf[wl, ncol - 1]
  
  ## if the file is to be read in chunks
  ## try to find out how many lines it has 
  if (is.null (nspc))
    if (nlines > 0){ 
      nspc <- wc (summary(file)$description, "lines")
      if (is.null (nspc))
        stop ("failed guessing nspc.")
      else {
        cat ("Counted", nspc[1,1], "lines or ")
        nspc <- nspc[1,1] / length (wl)
        cat (nspc, "spectra.\n")
      }
    } else {
      nspc <- nrow (fbuf) / length (wl)
    }
  
  data <- matrix (NA, ncol = ncol - 2, nrow = nspc)
  colnames (data) <- head (names (cols), -2) 
  pos.data <- 0
  
  spc <- numeric (nspc * length (wl))
  pos.spc <- 0
  
  while (length (fbuf > 0)){
    if (nlines > 0) cat (".")
    spc [pos.spc + seq_len (nrow (fbuf))] <- fbuf [, ncol]
    pos.spc <- pos.spc + nrow (fbuf)
    
    dummy <- fbuf [fbuf[, ncol - 1] == wl [1], seq_len (ncol - 2), drop = FALSE]
    
    data [pos.data + seq_len (nrow (dummy)), ] <- dummy
    pos.data <- pos.data + nrow (dummy)
    
    fbuf <- matrix (scan (file, quiet = TRUE, nlines = nlines), ncol = ncol, byrow = TRUE)
    
    if (length (fbuf > 0) & ! all(unique (fbuf[, ncol - 1]) %in% wl))
      stop ("Wavelengths do not correspond to that of the other chunks. Is the size of the first chunk large enough to cover a complete spectrum?")
  }
  if (nlines > 0) cat ("\n")
  
  spc <- matrix (spc, ncol = length (wl), nrow = nspc, byrow = TRUE)
  
  orderwl (new ("hyperSpec", spc = spc, data = as.data.frame (data), 
                wavelength = wl, label = cols, 
                log = list (short = "scan.txt.Renishaw",
                  long = list (file = file, cols = I (cols)), ...)
                )
           )
}


###-----------------------------------------------------------------------------
###
### write.txt.wide
###  
###
write.txt.wide <- function (object,
                            file = stop ("filename required"),
                            cols = NULL, 
                            quote = FALSE, sep = "\t",
                            row.names = FALSE,
                            col.names = TRUE,
                            header.lines = 1,   # 1 or 2 line header?
                            col.labels = if (header.lines == 1) FALSE else TRUE, # use labels instead of column names?
                            append = FALSE,
                            ...){
  validObject (object)
  
  if (! is.null (cols))
    object <- object [, cols]
  
  if (col.names){
    col.spc <- match ("spc", colnames (object@data))
    
    if (col.labels){
      cln <- match (colnames (object@data), names (object@label))
      cln[!is.na (cln)] <- object@label [cln[!is.na(cln)]]
      cln[is.na (cln)] <- colnames (object@data) [is.na(cln)]
      cln <- sapply (cln, as.character)
                                        #cln [-col.spc] <- object@label []
    } else {
      cln <- colnames (object@data)
    }
    
    i <- seq_along (cln)
    
    if (header.lines == 1){
      write.table (matrix (c(if (row.names) "" else NULL,
                             cln [i < col.spc],
                             object@wavelength,
                             cln [i > col.spc]
                             ), nrow = 1),
                   file = file, append = append, quote = quote, sep = sep, 
                   row.names = FALSE, col.names = FALSE)
      append = TRUE
    } else if (header.lines == 2) {
      ## 1st line          
      write.table (matrix (c (
                              if (row.names) "" else NULL, 
                              cln [i < col.spc],
                              if (col.labels) cln [col.spc] else "",
                              rep ("", length (object@wavelength) - 1),
                              cln [i > col.spc]), nrow = 1),
                   file = file, append = append, quote = quote, sep = sep, 
                   row.names = FALSE, col.names = FALSE)
      append = TRUE
      ## 2nd line
      write.table (matrix (c (if (row.names) (if (col.labels) as.character (object@label$.wavelength)
      else "wavelength")
      else NULL,
                              rep ("", sum (i < col.spc)),
                              object@wavelength,
                              rep ("", sum (i > col.spc))
                              ), nrow = 1),
                   file = file, append = append, quote, sep, 
                   row.names = FALSE, col.names = FALSE)
      
    } else {
      stop ("Only 1 or 2 line headers supported.")
    }
    
  }
  
  write.table (object@data, file = file, append = append, quote = quote, sep = sep,
               row.names = row.names, col.names = FALSE, ...) 
} 

###-----------------------------------------------------------------------------
###
### write.txt.long
###  
###
write.txt.long <- function (object,
                            file = stop ("filename required"),
                            order = c (".rownames", "wavelength"),
                            na.last = TRUE, decreasing = FALSE,
                            quote = FALSE, sep = "\t",
                            row.names = FALSE,
                            cols = NULL,
                            col.names = TRUE,
                            col.labels = FALSE, # use labels instead of column names?
                            append = FALSE,
                            ...){
  validObject (object)
  
  col.spc <- match ("spc", colnames (object@data))
#  i <- seq_len (ncol (object@data))
  
#  X <- array2df (object@data$spc, levels = list (n = seq_len (nrow (object)), .wavelength = object@wavelength),
#                 label.x = "spc")
#  X$.wavelength <- as.numeric(levels(X$.wavelength))[as.integer(X$.wavelength)]
  
#  X <- cbind (object@data[X$n, -col.spc, drop = FALSE], X[, c(".wavelength", "spc", "n")])

  X <- as.long.df (object, rownames = TRUE)
  
  if (!is.null (order)){
    if (is.character (order)) {
      dummy <- match (order, colnames (X))
      if (any (is.na (dummy)))
        stop ("write.txt.long: no such columns: ",
              paste (order [is.na (dummy)], collapse = ", "))
      order <- dummy
      }
    
    
    if (length (decreasing) < length (order))
      decreasing <- rep (decreasing, length.out = length (order))
    
    order.data <- as.list (X [, order, drop = FALSE])
    
    for (i in seq_along (order)){
      if (is.factor(order.data [[i]]))
        order.data [[i]] <- rank (order.data [[i]], na.last = na.last | is.na (na.last))
      
      if (decreasing [i])
        order.data [[i]] <- - order.data [[i]]
    }
    
    X <- X[do.call ("order",
                    c (order.data, na.last = na.last | is.na (na.last),	decreasing = FALSE)
                    ), ]
  }
  
  if (is.na (na.last))
    X <- X[! is.na (X$spc), ]

  if (!is.null (cols))
    X <- X [, cols, drop = FALSE]
      
  if (!row.names)
    X$.rownames <- NULL
  else
    cln [match (".rownames", cln)] <- "row"

  if (col.names){
    if (col.labels){
      cln <- match (colnames (X), names (object@label))
      cln[!is.na (cln)] <- object@label [cln[!is.na(cln)]]
      cln[is.na (cln)] <- colnames (X) [is.na(cln)]
      cln <- sapply (cln, as.character)
    } else {
      cln <- colnames (X)
    }

    
#    cln <- c(if (row.names) "row" else NULL,
#             cln #[i <= col.spc],
#                                        #if (col.labels) as.character (object@label$.wavelength) else "wavelength",
#                                        #cln [i > col.spc]
#             )

    write.table (matrix (cln, nrow = 1), file = file, append = append,
                 quote = quote, sep = sep, row.names = FALSE, col.names = FALSE)
    append = TRUE
  }
  
  write.table (X, file, append = append, quote = quote, sep = sep,
               row.names = FALSE, col.names = FALSE, ...) 
}

### ****************************************************************************
###
###  spectra-related functions
###  
### ****************************************************************************


###-----------------------------------------------------------------------------
###
###  spc.bin
###
spc.bin <- function (spc,
                     by = stop ("reduction factor needed"), na.rm = TRUE,
                     ...) {
  validObject (spc)
  
  long.description <- list (by = deparse (by), na.rm = na.rm)
  
  n <- ceiling (nwl (spc) / by)
  
  small <- nwl (spc) %% by 
  if (small != 0) 
    warning (paste (c("Last data point averages only ", small, " points.")))
  
  bin <- rep (seq_len (n), each = by, length.out = nwl (spc))	
  
  na <- is.na (spc@data$spc)
  
  if ((na.rm > 0) && any (na)) {
    if (na.rm == 1) { 
      na <- apply (!na, 1, tapply, bin, sum, na.rm = FALSE)
      spc@data$spc <- t (apply (spc@data$spc, 1, tapply, bin, sum, na.rm = TRUE) / na)
    } else { # faster for small numbers of NA
      dummy <- t (apply (spc@data$spc, 1, tapply, bin, sum, na.rm = FALSE))
      dummy <- sweep (dummy, 2, rle (bin)$lengths, "/")
      
      na <- which (is.na (dummy), arr.ind = TRUE)
      bin <- split (seq_len (ncol (spc@data$spc)), bin)
      
      for (i in seq_len (nrow (na))){
        dummy [na [i, 1], na [i, 2]] <- mean (spc@data$spc [na [i, 1], bin [[na[i, 2]]]], na.rm = TRUE)
      }
      spc@data$spc <- dummy
    }
  } else {  # considerably faster
    spc@data$spc <- t (apply (spc@data$spc, 1, tapply, bin, sum, na.rm = FALSE))
    spc@data$spc <- sweep (spc@data$spc, 2, rle (bin)$lengths, "/")
  }
  
  .wl (spc) <- as.numeric (tapply (spc@wavelength, bin, mean, na.rm = na.rm > 0))
  
  spc@log <- .logentry (spc,
                       long = long.description,
                       ...            # date and user?
                       )
  
  validObject (spc)
  
  spc
}
###-----------------------------------------------------------------------------
###
###  spc.fit.poly
###  
###
spc.fit.poly <- function (fit.to, apply.to = NULL, poly.order = 1, short = NULL, user = NULL, date = NULL){
  validObject (fit.to)
  validObject (apply.to)
  
  x <- fit.to@wavelength
  x <- outer(x, 0 : poly.order, "^")             # Vandermonde matrix of x 
  p <- apply (fit.to, 1, function (y, x){qr.solve (x, y)}, x,
              short = if (is.null (short)) "spc.fit.poly: coefficients" else short,
              user = user, date = date)
  
  if (is.null (apply.to)){
    colnames (p@data$spc) <- paste ("x^", 0 : poly.order, sep="")
    
    p
  } else {
    wl <- apply.to@wavelength;
    x <- outer(wl, 0 : poly.order, "^")             # Vandermonde matrix of x
    apply.to@data$spc <- I (t (apply (p[[]], 1, function (p, x) {x %*% p}, x)))
    apply.to@log <- .logentry (apply.to, 		
                              short = if (is.null (short)) "spc.fit.poly: spectra" else short,
                              long = list (apply = match.call()$apply, poly.order = poly.order), 
                              user = user, date = date)
    
    
    .wl(apply.to) <- wl
    colnames (apply.to@data$spc) <- format (wl, digits = 4)
    
    apply.to
  }
}

                                        #)

###-----------------------------------------------------------------------------
###
###  spc.fit.poly.below
###  
###

spc.fit.poly.below <- function (fit.to, apply.to = fit.to, poly.order = 1, npts.min = NULL,
                                noise = 0, short = NULL, user = NULL, date = NULL){
  
  validObject (fit.to)
  validObject (apply.to)
  
  if (is.null (npts.min)){
    npts.min <- max (round (nwl(fit.to) * 0.05), 3 * (poly.order + 1))
    cat ("Fitting with npts.min = ",  npts.min, "\n") 
  } else  if (npts.min <= poly.order){
    npts.min <- poly.order + 1
    warning (paste ("npts.min too small: adjusted to", npts.min))
  }
  
  if (length (noise) == 1)
    noise <- rep (noise, nrow (fit.to))
  
  vdm <- outer(fit.to@wavelength, 0 : poly.order, "^")
  y <- t(fit.to [[]])
  
  p <- matrix (nrow = nrow(fit.to) , ncol = poly.order + 1)
  for (i in seq_len (nrow (fit.to))){
    use.old <- logical (nwl (fit.to))
    use <- !use.old
    
    repeat {
      p[i,] <- qr.solve (vdm[use,], y[use, i]) 
      bl <- vdm %*% p [i,]
      use.old <- use
      use <- y[, i] < bl + noise [i]
      if ((sum (use) < npts.min) || all (use == use.old))
        break
    }
  }
  if (is.null (apply.to)){
    fit.to@data$spc <- p
    .wl (fit.to) <- 0 : poly.order
    colnames (fit.to@data$spc) <- paste ("x^", 0 : poly.order, sep="")
    fit.to@log <- .logentry (fit.to, short = if (is.null (short)) "spc.fit.poly.below: coefficients" else short, 
                            long = list (apply = NULL, poly.order = poly.order,
                              npts.min = npts.min, noise = noise), 
                            user = user, date = date)
    fit.to
  } else {
    x <- apply.to@wavelength
    
    vdm <- outer(x, 0 : poly.order, "^")             # Vandermonde matrix of x
    
    apply.to@data$spc <- I (t (apply (p, 1, function (p, x) {x %*% p}, vdm)))
    apply.to@log <- .logentry (apply.to, 		
                              short = if (is.null (short)) "spc.fit.poly.below: spectra" else short,
                              long = list (apply = match.call()$apply, poly.order = poly.order,
                                npts.min = npts.min, noise = noise), 
                              user = user, date = date)
    
    .wl(apply.to) <- x
    colnames (apply.to@data$spc) <- format (x, digits = 4)
    apply.to
  }
}

###-----------------------------------------------------------------------------
###
###  spc.loess
###  
###  
spc.loess <- function (spc, newx, ..., 
                       short = NULL, user = NULL, date = NULL){
  validObject (spc)	
  
  if (any (newx < min (spc@wavelength)) || any (newx > max (spc@wavelength)))
    warning ("newx outside spectral range of spc. NAs will be generated.")
  
  dots <- list (...)
  if (is.null (dots$enp.target))
    dots$enp.target <- nwl (spc) / 4
  
  if (is.null (dots$surface))
    dots$surface <- "direct"
  
  loess <- apply (t (spc[[]]), 2, 
                  function (y, x){
                    do.call (loess, c(y ~ x, dots))	
                  }, 
                  spc@wavelength)
  
  spc@data$spc <- t (sapply (loess, predict, newx))
  .wl(spc) <- newx
  
  spc@log <- .logentry (spc, 		
                       short = if (is.null (short)) "spc.loess" else short,
                       long = list (newx = newx, enp.target = dots$enp.target,  ...),  
                       user = user, date = date)
  
  spc
}

### ****************************************************************************
###
###  some more convenient functions, not operating on hyperSpec objects
###  
### ****************************************************************************

###-----------------------------------------------------------------------------
###
###  vec2array -- vector index to array index conversion
###  
###  

vec2array <- function (ivec, dim) {
  ndim <- length (dim)
  pdim <- c(1, cumprod (dim))
  
  iarr <- matrix(NA, nrow = length(ivec), ncol = ndim) # matrix for the array indices
  colnames (iarr) <- letters[8 + seq_len (ndim)]       # i, j, k, ...
  
  ivec <- (ivec - 1)
  for (j in seq_len (ndim))
    iarr [, j] <- (ivec %% pdim [j + 1]) / pdim [j]
  
  1 + floor(iarr)
}

###-----------------------------------------------------------------------------
###
###  array2vec -- array index to vector index conversion
###  
###  

array2vec <- function (iarr, dim){
  if (!is.matrix (iarr))
    dim (iarr) <- c(1, length (iarr))
  
  if (ncol (iarr) != length (dim))
    stop ("Number of columns in iarr and number of dimensions differ.")
  
  if (any (sweep (iarr, 2, dim) > 0))
    stop ("array index > dim")
  
  pdim <- c(1, cumprod (dim [- length (dim)]))
  iarr <- iarr - 1
  
  colSums(apply (iarr, 1, "*", pdim)) + 1
} 

###-----------------------------------------------------------------------------
###
###  array2df -- "explodes" a mulitdimensional array into a long form matrix or
###              data.frame. Compare stack, unstack.
###  
###  

array2df <- function (x, levels = rep (NA, length (dims)),
                      matrix = FALSE,
                      label.x = deparse (substitute (x))){
  dims  <- c(dim (x))
  cprod <- c(1, cumprod (dims))
  rprod <- c(rev (cumprod (rev (dims))), 1)[-1]
  idim  <- seq_len (length (dims)) [! sapply (levels, is.null)]
  
  df <- matrix (x, nrow = length (x), ncol = length (idim) + 1)
  
  for (d in seq (along = idim))
    df [, d + 1] <-  rep (seq_len (dims [idim [d]]), each = cprod [idim [d]], times = rprod [idim [d]])
  
  if(!matrix){
    df <- as.data.frame (df)
    
    for (d in seq (along = idim)){
      if (! all (is.na(levels[[idim [d]]]))){
        df[, d + 1] <- factor (df[, d + 1], labels = levels [[idim [d]]])
      }
    }
    
  }
  colnames (df) <- c (label.x, names (levels)[idim])
  
  df
}

###-----------------------------------------------------------------------------
###
###  matlab.palette
###  
###  

matlab.palette <- function (n = 100) {
  rev (rainbow (n, start = 0, end = 4/6))
}

###-----------------------------------------------------------------------------
###
###  matlab.dark.palette
###  
###  

matlab.dark.palette <- function (n = 100) {
  pal <- rev (rainbow (n, start = 0, end = 4/6))
  pal <- col2rgb(pal)
  pal ["green",] <- pal ["green",] / 2
  
  rgb (t (pal)/255)
}

###-----------------------------------------------------------------------------
###
### pearson.dist
###  
### 

pearson.dist <- function (x) {
  as.dist (0.5 - cor (t(x)) / 2) 
}

###-----------------------------------------------------------------------------
###
### mean_pm_sd
###  
###  

mean_pm_sd <- function (x, na.rm = TRUE){
  m <- mean (x, na.rm = na.rm)
  s <- sd (x, na.rm = na.rm)
  c(m - s, m, m + s)
}

###-----------------------------------------------------------------------------
###
### mean_sd
###  
###  

mean_sd <- function (x, na.rm = TRUE)
  c(mean (x, na.rm = na.rm),  sd (x, na.rm = na.rm))



                                        #' word count 
                                        #' 
                                        #' wc uses the system command wc
                                        #' 
                                        #' @param file the file name or pattern
                                        #' @param flags the parameters to count, character vector with the long form of the parameters 
                                        #' @return data.frame with the counts and file names, or \code{NULL} if wc is not available
                                        #' @author cb
                                        #' @export
wc <- function (file, flags = c("lines", "words", "bytes")){
  if (length (system ("wc --help", intern = TRUE)) == 0)
    return (NULL)
  
  wc <- paste ("wc", paste ("--", flags, sep = "", collapse = ", "), file)
  wc <- read.table(pipe (wc))
  colnames (wc) <- c(flags, "file")
  wc
} 

