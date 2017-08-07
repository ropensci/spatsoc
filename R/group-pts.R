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
#'
#' @return
#' @export
#'
#' @examples
GroupPts <- function(dt, bufferWidth, crs, coordFields = c('EASTING', 'NORTHING'),
                     idField = 'ID', spPts = NULL){
  if(is.null(spPts)){
    if(is.null(dt)) stop("must provide either pts or dt")
    spPts <- BuildPts(dt, crs, coordFields, idField)
  }

  buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
  o <- sp::over(spPts, sp::disaggregate(buffers))
  dt <- data.table::data.table(spPts@coords, id = spPts$id, group = o)
}
