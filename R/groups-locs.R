#' Group Points By Buffer
#'
#' Group points by buffer overlap at each time interval
#'
#' this function uses input spatial points to determine
#'
#' @param sp.pts The SpatialPointsDataFrame as created by the sp package. The
#' @param width The width of the buffer in the units of the projection
#'
#' @return
#' @export
#'
#' @examples
grp.pts <- function(sp.pts, width){
  buffers <- rgeos::gBuffer(sp.pts, width=buffer.width, byid = FALSE)
  o <- sp::over(sp.pts, sp::disaggregate(buffers))
  dt <- data.table::data.table(sp.pts@coords, id = sp.pts$id, group = o)
}


# this function is missing the time by

