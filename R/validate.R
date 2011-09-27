.validate <- function (object) {
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
