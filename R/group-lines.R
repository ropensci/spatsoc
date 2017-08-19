#' Groups From Spatial Lines
#'
#' @inheritParams BuildPts
#' @param bufferWidth The width of the buffer around the geometry in the units
#'   of the projection. Optionally, exclude argument or supply 0 to compare
#'   SpatialLines intersection
#' @param spLines Alternatively, provide a SpatialLines object created with the sp
#'   package. If a spLines object is provided, groups cannot be calculated by a
#'   timeField
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' data(locs)
#' groups <- GroupLines(locs, 50)
#'
#' groups <- GroupLines(locs, 50, timeField = 'FIX_DATE',
#'         projection = '+proj=utm +zone=21 ellps=WGS84',
#'         idField = 'ID')
#'
#'
#' data(locsLines)
#'
#' groups <- GroupLines(spLines = locsLines)
#' @import data.table
GroupLines <- function(dt, bufferWidth = 0, timeField = NULL, projection, coordFields = c('EASTING', 'NORTHING'),
                       idField = 'ID', spLines = NULL) {
  # Check for a timeField
  if(is.null(timeField)){
    # Check if spLines is already provided
    if(is.null(spLines)){
      if(is.null(dt)) stop("must provide either spLines or dt")
      # If it isn't, build it
      spLines <- BuildLines(dt, projection, coordFields, idField)
    }
    if(bufferWidth == 0) {
      merged <- rgeos::gLineMerge(spLines)
    } else {
      merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
    }

    # Find which buffers overlap each other and return
    ovr <- sp::over(spLines, sp::disaggregate(merged), returnList = T)
    return(data.table::data.table(id = names(ovr), group = unlist(ovr)))
  } else {
    # If timeField is provided, check that spLines is not.
    if(!is.null(spLines)) {
      stop("if providing a spLines, cannot provide a time field")
    }

    # Build and buffer as above, by timeField. Return spatial and unique groups
    dt[, {spLines <- BuildLines(.SD, projection, coordFields, idField)
          if(bufferWidth == 0) {
            merged <- rgeos::gLineMerge(spLines)
          } else {
            merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
          }
          ovr <- sp::over(spLines, sp::disaggregate(merged), returnList = TRUE)
          list(id = names(ovr), spatialGroup = unlist(ovr))},
       by = timeField,
       .SDcols = c(coordFields, idField)][, group := .GRP, by = .(spatialGroup, get(timeField))][]
  }
}
# TODO: check above else for same error
# TODO: optional buffer
# TODO: check if the IDs are returned well with drop during build
# TODO: check here and others for if default coordFIelds and idField are not present...
