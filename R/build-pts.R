#' Build Points
#'
#' Build a SpatialPointsDataFrame from provided data for input to GroupPts or
#' other uses.
#'
#' The parameter projection can be built by finding the EPSG code for your
#' given projection at <http://spatialreference.org/ref/epsg/> and providing it with
#' "+init=epsg:CODE" (eg: "+init=epsg:4326").
#'
#' @param DT Input data with coordinate and id fields. If not provided, fields
#'   default to c('EASTING', 'NORTHING') and 'ID'
#' @param projection Character string for input to sp::CRS() and proj4string()
#' @param coordFields Character vector indicating the X coordinate and Y
#'   coordinate columns. eg: c('EASTING', 'NORTHING')
#' @param idField Character string indicating the column name for the ID
#'   associated with each point
#'
#' @return SpatialPointsDataFrame for each ID provided
#' @export
#'
#' @import data.table
BuildPts <- function(DT, projection, coordFields = c('EASTING', 'NORTHING'), idField = 'ID'){
  if(any(!(c(idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  sp::SpatialPointsDataFrame(DT[, ..coordFields],
                             proj4string = sp::CRS(projection),
                             data = DT[, .(id = get(idField))])
}

# TODO: check if DT is a data.table
# TODO: stopif null projection
