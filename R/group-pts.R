#' Group Points By Buffer
#'
#' Group points by buffer overlap at equal time.
#'
#' This function uses input spatial points to determine groups in space and
#' time.
#'
#' @inheritParams BuildPts
#' @param spPts Alternatively, provide a SpatialPointsDataFrame created with the
#'   sp package. If a spPts object is provided, groups cannot be calculated by
#' @param bufferWidth The width of the buffer around the geometry in the units of the projection
#' @param timeField (optional) time field in the dt upon which the spatial grouping will be
#'   calculated
#' @return id by group data.table
#' @export
#'
GroupPts <- function(dt, bufferWidth, timeField = NULL, crs, coordFields = c('EASTING', 'NORTHING'),
#' @examples
#' data(locs)
#' groups <- GroupPts(locs, 50)
#'
#' groups <- GroupPts(locs, 50, timeField = 'FIX_DATE',
#'         crs = '+proj=utm +zone=21 ellps=WGS84',
#'         idField = 'ID')
#'
#' data(locsPts)
#'
#' groups <- GroupPts(spPts = locsPts)
                     idField = 'ID', spPts = NULL){
  if(is.null(timeField)){
    if(is.null(spPts)){
      if(is.null(dt)) stop("must provide either pts or dt")
      spPts <- BuildPts(dt, crs, coordFields, idField)
    }
    buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
    ovr <- sp::over(spPts, sp::disaggregate(buffers))
    dt <- data.table::data.table(spPts@coords, id = spPts$id, spatialGroup = ovr)
  } else {
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")
    dt[, {spPts <- BuildPts(.SD, crs, coordFields, idField)
          buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
          ovr <- sp::over(spPts, sp::disaggregate(buffers))
          c(as.list(as.data.frame(spPts@coords)),
            list(id = spPts$id, spatialGroup = ovr))},
       by = timeField,
       .SDcols = c(coordFields, idField)][, group := .GRP, by = .(spatialGroup, get(timeField))][]
  }
}
