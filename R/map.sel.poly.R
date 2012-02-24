##' Interactively select a polygon (grid graphics) and highlight points
##'
##' Click the points that should be connected as polygon. Input ends with right click (see
##' \code{\link[grid]{grid.locator}}). Polygon will be drawn closed. Wrapper for 
##' \code{\link{plotmap}}, \code{\link{sel.poly}}, and \code{\link[sp]{point.in.polygon}}.
##' @param data hyperSpec object for plotting map
##' @param pch symbol to display the points of the polygon for \code{\link{sel.poly}}
##' @param size size for polygon point symbol for \code{\link{sel.poly}}
##' @param \dots further arguments for \code{\link[grid]{grid.points}} and
##' \code{\link[grid]{grid.lines}}
##' @return array of indices for points within selected polygon
##' @author Claudia Beleites, Sebastian Mellor
##' @seealso \code{\link[grid]{grid.locator}},  \code{\link{sel.poly}}, \code{\link{map.identify}}
##' @export
##' @rdname map-sel-poly
##' @keywords iplot
##' @examples
##' \donttest{
##' map.sel.poly (chondro)
##' }
map.sel.poly <- function (data, pch = 19, size = 0.3, ...){
  print (plotmap (data))
  
  poly <- sel.poly (pch = pch, size = size, ...)
  
  if (! require ("sp"))  stop ("Error: sp package required for point.in.polygon ()")

  pts <- point.in.polygon (data$x, data$y, poly [, 1], poly [, 2]) #mode.checked=F

  ind <- pts > 0

  if (! any (ind))
    warning ("Empty selection: no point in polygon.")

  ind
}

