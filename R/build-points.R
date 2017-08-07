#' Build Points
#'
#' @param dt Input data with coordinate and id fields. If not provided, fields default to c('EASTING', 'NORTHING') and 'ID'
#' @param crs Character string for input to sp::CRS()
#' @param coord.fields Character vector indicating the X coordinate and Y
#'   coordinate columns. eg: c('EASTING', 'NORTHING')
#' @param id.field Character string indicating the column name for the ID associated
#'   with each point
#'
#' @return SpatialPointsDataFrame for each ID provided
#' @export
#'
#' @examples
#' @import data.table
build_pts <- function(dt, crs, coord.fields = c('EASTING', 'NORTHING'), id.field = 'ID'){
  # TODO: check if dt is a data.table
  # TODO: stopif null crs
  sp::SpatialPointsDataFrame(dt[, ..coord.fields],
                             proj4string = sp::CRS(crs),
                             data = dt[, .(id = get(id.field))])
}
