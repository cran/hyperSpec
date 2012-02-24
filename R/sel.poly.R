##' Interactively select a polygon (grid graphics)
##'
##' Click the points that should be connected as polygon. Input ends with right click (see
##' \code{\link[grid]{grid.locator}}).
##' @param pch symbol to display the points of the polygon
##' @param size size for polygon point symbol
##' @param \dots further arguments for \code{\link[grid]{grid.points}} and
##' \code{\link[grid]{grid.lines}}
##' @return n x 2 matrix with the points of the open polygon
##' @author Claudia Beleites
##' @seealso \code{\link[grid]{grid.locator}}
##' @export
##' @keywords iplot
##' @rdname map.sel.poly
sel.poly <- function (pch = 19, size = 0.3, ...){
  trellis.focus ()

  pts <- matrix (NA, nrow = 0, ncol = 2)
  
  repeat {
    pt <- grid.locator (unit="native")
    if (!is.null (pt)){
      pts <- rbind (pts, as.numeric (pt)) # comparably few executions: low performance doesn't matter

      ## display the clicked point
      grid.points (unit (tail (pts [, 1], 1), "native"),
                   unit (tail (pts [, 2], 1), "native"), pch = pch,
                   size = unit (size, "char"), gp = gpar (...))

      ## connect last 2 points by line
      if (nrow (pts) > 1L)
        grid.lines (unit (tail (pts [, 1L], 2L) , "native"),
                    unit (tail (pts [, 2L], 2L) , "native"), gp = gpar (...))
    } else {
      ## visually close polygon (if at least 3 pts)
      if (nrow (pts) > 2L)
        grid.lines (unit (c (tail (pts [, 1L], 1L), pts [1L, 1L]), "native"),
                    unit (c (tail (pts [, 2L], 1L), pts [1L, 2L]), "native"), gp = gpar (...))
      break                            
    }
  }
  
  pts
}
