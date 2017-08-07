#' Group Points By Buffer
#'
#' Group points by buffer overlap at each time interval
#'
#' This function uses input spatial points to determine groups in space and time.
#'
#' @inheritParams build_pts
#' @param sp.pts Alternatively, provide a SpatialPointsDataFrame created with the sp package.
#' @param buffer.width The width of the buffer in the units of the projection
#'
#' @return
#' @export
#'
#' @examples
grp_pts <- function(dt, buffer.width, crs, coord.fields = c('EASTING', 'NORTHING'),
                    id.field = 'ID', sp.pts = NULL){
  if(is.null(sp.pts)){
    if(is.null(dt)) stop("must provide either sp.pts or dt")
    sp.pts <- build_pts(dt, crs, coord.fields, id.field)
  }

  buffers <- rgeos::gBuffer(sp.pts, width = buffer.width, byid = FALSE)
  o <- sp::over(sp.pts, sp::disaggregate(buffers))
  dt <- data.table::data.table(sp.pts@coords, id = sp.pts$id, group = o)
}
