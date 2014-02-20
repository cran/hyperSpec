##' @rdname read.jdx
##' @deprecated 
##' @export
read.jdx.Shimadzu <- function (filename, encoding=""){
  .Deprecated (read.jdx (filename, encoding,
                         header = list (xunits = expression (m/z), yunits = 'I / a.u.')),
               package = "hyperSpec",
               msg = "read.jdx.Shimadzu is deprecated.\nUse\nread.jdx (filename, encoding, header = list (xunits = expression (m/z), yunits = 'I / a.u.'))")
  
  jdx <- readLines (filename, encoding = encoding)

  spcstart <-  grep ("^##XYDATA=[[:blank:]]*[(]XY[.][.]XY[)][[:blank:]]*$", jdx) + 1
  spcend <- grep ("^##END=[[:blank:]]*$", jdx) - 1
  metastart <- c (1, head (spcend, -1) + 1)

  stopifnot (length (spcstart) == length (spcend))
  stopifnot (all (spcstart < spcend))

  spc <- list ()
  for (s in seq_along (spcstart)){
    ## read.txt.long produces hyperSpec object
    spc [[s]] <- read.txt.long  (textConnection (jdx [spcstart [s] : spcend [s]]),
                                 cols = list(.wavelength = "m/z", spc = "I / a.u."),
                                 header = FALSE, sep = ",")

    ## look for metadata
    meta <- jdx [metastart [s] : (spcstart [s] - 2)]

    CASname <- grepl ("##CAS NAME=", meta)
    if (any (CASname))
      spc[[s]]$CASname <- .trimquotes (gsub ("##CAS NAME=(.*)$", "\\1", meta [CASname]))

    ## add processing of further metadata here.
  }

  if (length (spc) > 1L)  
    spc <- collapse (spc)

  .logentry (spc, short = "read.jdx.Shimadzu", long = list (filename = filename))
}
.trimquotes <- function (x){
  gsub ("[\"'[:blank:]]*([^\"'[:blank:]].*[^\"'[:blank:]])[\"'[:blank:]]", "\\1", x)
}


