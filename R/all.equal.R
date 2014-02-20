##' @aliases all.equal  all.equal,hyperSpec,hyperSpec-method
##' @rdname Comparison
##' @param target,current two \code{hyperSpec} objects that are tested for
##'   equality
##' @param \dots handed to \code{\link[base]{all.equal}} when testing the slots of the
##'   \code{hyperSpec} objects
##' @param check.column.order If two objects have the same data, but the order
##'   of the columns (determined by the names) differs, should they be regarded
##'   as different?
##' @param check.label Should the slot \code{label} be checked? \cr If the
##'   labels differ only in the order of their entries, they are conidered
##'   equal.
##' @param check.log Should the slot \code{label} be checked?
##' @param check.attributes,check.names see \code{\link[base]{all.equal}}
##' @return \code{all.equal} returns either \code{TRUE}, or a character vector describing the
##' differences. In conditions, the result must therefore be tested with
##' \code{\link[base]{isTRUE}}.
##' @seealso \code{\link[base]{all.equal}} and \code{\link[base]{isTRUE}}
##' @export
##' @examples
##' 
##' all.equal (flu, --flu);
setMethod ("all.equal", signature (target = "hyperSpec", current = "hyperSpec"),
           function (target, current, ..., check.attributes = FALSE, check.names = FALSE, 
                     check.column.order = FALSE, check.label = FALSE, check.log = FALSE){
             validObject (target)
             validObject (current)

             result <- character (0)

             cmp <- all.equal (target = target@wavelength, current = current@wavelength, ...,
                               check.attributes = check.attributes, check.names = check.names)
             if (! isTRUE (cmp)) result <- c("@wavelength:", cmp)

             if (check.column.order)
               cmp <- all.equal (target = target@data, current = current@data, ...,
                                 check.attributes = check.attributes)
             else
               cmp <- all.equal (target  = target@data  [order (colnames ( target@data))],
                                 current = current@data [order (colnames (current@data))],
                                 ...,
                                 check.attributes = check.attributes, check.names = check.names)
             if (! isTRUE (cmp)) result <- c (result, "@data:", cmp)

             if (check.label){
               cmp <- all.equal (target  = target@label  [order (names (target@label))],
                                 current = current@label [order (names (current@label))],
                                 ...,
                                 check.attributes = check.attributes, check.names = check.names)
               if (! isTRUE (cmp)) result <- c (result, "@label:", cmp)
             }

             if (check.log) {
               warning ("hyperSpec's logbook is deprecated and will be removed.")
             
               cmp <- all.equal (target = target@log, current = current@log, ...,
                                 check.attributes = check.attributes, check.names = check.names)
               if (! isTRUE (cmp)) result <- c (result, "@log:", cmp)
             }

             if (length (result) == 0)
               TRUE
             else
               result
           }
           )

