##' Import Raman Spectra/Maps from Witec Instrument via ASCII files
##'
##' 
##' @title File Import Witec Raman
##' @param filex filename wavelength axis file
##' @param filey filename intensity file
##' @param points.per.line number of spectra in x direction of the map
##' @param lines.per.image number of spectra in y direction 
##' @param short,user,date handed to \code{\link{logentry}}
##' @return a hyperSpec object
##' @author Claudia Beleites
##' @seealso \code{vignette ("fileio")} for more information on file import.
##' @export 
scan.txt.Witec <- function (filex = stop ("filename needed"),
                            filey = sub ("-x", "-y", filex),
                            points.per.line = NULL,
                            lines.per.image = NULL,
                            short = "scan.txt.Witec", user = NULL, date = NULL){
  wl <- scan (file = filex)
  spc <- scan (file = filey)

  dim (spc) <- c (length (wl), length (spc) / length (wl))

  spc <- new ("hyperSpec", wavelength = wl, spc = t (spc))

  if (!is.null (points.per.line))
    spc@data$x <- rep (seq_len (points.per.line), lines.per.image)

  if (!is.null (points.per.line))
    spc@data$y <- rep (- seq_len (lines.per.image), each = points.per.line)

  spc
}
