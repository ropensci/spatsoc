#' Build Points
#'
#' Build a SpatialPointsDataFrame from provided data for input to GroupPts or other uses.
#'
#' @param dt Input data with coordinate and id fields. If not provided, fields default to c('EASTING', 'NORTHING') and 'ID'
#' @param crs Character string for input to sp::CRS()
#' @param coordFields Character vector indicating the X coordinate and Y
#'   coordinate columns. eg: c('EASTING', 'NORTHING')
#' @param idField Character string indicating the column name for the ID associated
#'   with each point
#'
#' @return SpatialPointsDataFrame for each ID provided
#' @export
#'
#' @import data.table
BuildPts <- function(dt, crs, coordFields = c('EASTING', 'NORTHING'), idField = 'ID'){
  sp::SpatialPointsDataFrame(dt[, ..coordFields],
                             proj4string = sp::CRS(crs),
                             data = dt[, .(id = get(idField))])
}

# TODO: check if dt is a data.table
# TODO: stopif null crs
