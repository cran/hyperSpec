##' Append a Row to the logbook of a hyperSpec Object
##' A log entry is generated and appended to the log of \code{x}.
##' 
##' The arguments (besides x) go into the respective columns of \code{x@@log}.
##' 
##' The following values are used for any arguments that are \code{NULL}:
##' 
##' The name of the funtion that called \code{logentry} is used for
##' \code{short}, and its call for \code{long}.
##' 
##' For \code{date}, the current \code{\link[base]{Sys.time}()} is used, and
##' \code{user} is constructed from \code{\link[base]{Sys.info}()}'s
##' \code{user} and \code{nodename}.
##' 
##' @param x a \code{hyperSpec} object
##' @param short short description
##' @param long long description, e.g. list with function arguments
##' @param date time stamp
##' @param user username
##' @return \code{x@@log} (\emph{\code{data.frame}}) including the new row.
##' @export
##' @author C. Beleites
##' @examples
##' 
##' logentry (chondro, short = "test")
##' 
logentry <- function (x, short = NULL, long = NULL, date = NULL, user = NULL){
  chk.hy (x)
  validObject (x)

  .logentry (x, short = short, long = long, date = date, user = user)
}

###-------------------------------------------------------------------------------
###
###  .logentry - create new log item (hyperSpec)
###
###
##' @include call.list.R
##' @nord
.logentry <- function (x, ..., .entry = NULL){
  if (.options$log){
    .entry <- c (.entry, list (...))
    .entry <- .entry [! sapply (.entry, is.null)]
    
    Call <- sys.call (-1)
    Call <- list (short = Call [[1]],
                  long = .call.list (Call),
                  date = Sys.time (),
                  user = paste (Sys.info()[c("user", "nodename")], collapse= "@")
                  )
    
    .entry <- modifyList (Call, .entry)
                       
    x@log <- rbind (x@log, data.frame (short.description = as.character (.entry$short),
                                       long.description = I(list (.entry$long)),
                                       date = .entry$date,
                                       user = .entry$user
                                       )
                    )
  }

  x
}
