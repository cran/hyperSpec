#################################################################################
###
###  levelplot.R - everything that has to do with levelplot-like plotting:
###
###  levelplot is used by plotmap, plotvoronoi
###

### the workhorse function
.levelplot <- function (x, data, transform.factor = TRUE, ...,
                        contour = FALSE, useRaster = !contour) {
  validObject (data)

  data$.row <- row.seq (data)

  ## parse formula to find the columns to be plotted
  ## they may include also "wavelength"
  parsed.formula <- latticeParseFormula (x,
        as.long.df (data [1, , 1, wl.index = TRUE], rownames = TRUE),
        dimension = 3)
  use.x <- parsed.formula$right.x.name
  use.y <- parsed.formula$right.y.name
  use.z <- parsed.formula$left.name

  dots <- list (..., contour = contour, useRaster = useRaster)
  
  ## if spc is used as z and the data set has multiple wavelengths cut and warn
  if (use.z == "spc" && nwl (data) > 1 &&
      !any (grepl (".wavelength", c(as.character (x),
                                    as.character (dots$groups),
                                    as.character (dots$subset))))) {
    data <- data [, , 1, wl.index = TRUE]
    warning ("Only first wavelength is used for plotting")
  }
  
  dots <- modifyList (list (xlab = data@label [[use.x]],
                            ylab = data@label [[use.y]]),
                      dots)

  if (any (grepl ("spc", c(as.character (x),
                           as.character (dots$groups),
                           as.character (dots$subset))))){
    data <- as.long.df (data, rownames = TRUE,
                        wl.factor =  ".wavelength" %in% c (as.character (dots$groups),
                                                           as.character (dots$subset),
                                                           names (parsed.formula$condition))
                        )
  } else {
    data <- data$..
    data$.rownames <- as.factor (rownames (data))
  }


  
  if (is.factor (data [[use.z]]) && transform.factor) {
    dots <- trellis.factor.key (data [[use.z]], dots)
    data [[use.z]] <- as.numeric (data [[use.z]])
  }
  
  do.call(levelplot, c (list (x, data), dots))
}

##' @include plotmap.R
##' @rdname levelplot
##' @import lattice
##' @param transform.factor If the color-coded variable is a factor, should
##'   \code{\link{trellis.factor.key}} be used to compute the color coding and
##'   legend?
##' @param contour,useRaster see  \code{\link[lattice]{levelplot}}
##' @export
##' @seealso  \code{\link[lattice]{levelplot}}
##'
##'  \code{\link{trellis.factor.key}} for improved color coding of factors
setMethod ("levelplot", signature = signature (x = "formula", data = "hyperSpec"), .levelplot)

##' @rdname levelplot
##' @export

setMethod ("levelplot", signature = signature (x = "hyperSpec", data = "missing"),
           function (x, data, ...) {
             .levelplot (x = formula (spc ~ .wavelength * .row), data = x, ...)
           })
