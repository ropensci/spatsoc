#' Groups From Spatial Lines
#'
#' @param buffer.width The width of the buffer around the SpatialLines in the
#'   units of the projection provided.
#' @inheritParams build_pts
#'
#' @return id by spatial group data.table
#' @export
#'
#' @examples
#'
#' @import data.table
grp_lines <- function(dt, buffer.width, crs, coord.fields = c('EASTING', 'NORTHING'),
                      id.field = 'ID', sp.lines = NULL) {
  if(is.null(sp.lines)){
    if(is.null(dt)) stop("must provide either sp.lines object or dt")

    sp.lines <- build_lines(dt, crs, coord.fields, id.field)
  }

  # Buffer those lines by a preassigned buffer width
  buffers <- rgeos::gBuffer(sp.lines, width = buffer.width, byid = FALSE)

  # Find which buffers overlap each other and return as a list
  ovr <- sp::over(sp.lines, sp::disaggregate(buffers), returnList = T)
  return(data.table::data.table(id = names(ovr), group = unlist(ovr)))
}
