#' Build Points
#'
#' Build a SpatialPointsDataFrame from provided data.table.
#'
#' The parameter projection can be built by finding the EPSG code for your
#' given projection at <http://spatialreference.org/ref/epsg/> and providing it with
#' "+init=epsg:CODE" (eg: "+init=epsg:4326").
#'
#' @param DT input data.table
#' @param projection PROJ.4 charaster string
#' @param coords Character vector of X coordinate and Y coordinate column names
#' @param id Character string of ID column name
#' @return SpatialPointsDataFrame
#' @export
#'
BuildPts <- function(DT, projection, coords = c('EASTING', 'NORTHING'), id = 'ID'){
  if(any(!(c(id, coords) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  sp::SpatialPointsDataFrame(DT[, ..coords],
                             proj4string = sp::CRS(projection),
                             data = DT[, .(id = get(id))])
}

