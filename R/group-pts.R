#' Group Points By Buffer
#'
#' Group points by buffer overlap at each time interval
#'
#' This function uses input spatial points to determine groups in space and
#' time.
#'
#' @inheritParams BuildPts
#' @param spPts Alternatively, provide a SpatialPointsDataFrame created with the
#'   sp package.
#' @param bufferWidth The width of the buffer in the units of the projection
#' @param timeField Time field in the dt upon which the spatial grouping will be calculated
#' @return
#' @export
#'
#' @examples
GroupPts <- function(dt, bufferWidth, timeField, crs, coordFields = c('EASTING', 'NORTHING'),
                     idField = 'ID', spPts = NULL){
  if(is.null(timeField)){
    if(is.null(spPts)){
      if(is.null(dt)) stop("must provide either pts or dt")
      spPts <- BuildPts(dt, crs, coordFields, idField)
    }
    GroupSpatial(spPts, 'pts', bufferWidth = bufferWidth)
  } else {
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")
    dt[, GroupSpatial(BuildPts(.SD, crs, coordFields, idField),
                      'pts', bufferWidth)
       by = timeField][, .(..c(timeField, coordFields, idField#### chain and keep + uniqueGroup))]
  }
}
