#' Groups From Spatial Lines
#'
#' @param bufferWidth The width of the buffer around the SpatialLines in the
#'   units of the projection provided.
#' @inheritParams BuildPts
#'
#' @return id by spatial group data.table
#' @export
#'
#' @examples
#'
#' @import data.table
GroupLines <- function(dt, bufferWidth, crs, coordFields = c('EASTING', 'NORTHING'),
                      idField = 'ID', spLines = NULL) {
  if(is.null(spLines)){
    if(is.null(dt)) stop("must provide either spLines or dt")

    spLines <- BuildLines(dt, crs, coordFields, idField)
  }

  # Buffer those lines by a preassigned buffer width
  buffers <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)

  # Find which buffers overlap each other and return as a list
  ovr <- sp::over(spLines, sp::disaggregate(buffers), returnList = T)
  return(data.table::data.table(id = names(ovr), group = unlist(ovr)))
}
