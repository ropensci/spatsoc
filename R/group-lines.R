#' Groups From Spatial Lines
#'
#' @inheritParams BuildPts
#' @inheritParams GroupPts
#' @spLines Alternatively, provide a SpatialLines object created with the sp
#'   package. If a spLines object is provided, groups cannot be calculated by a
#'   timeField
#' @return id by spatial group data.table
#' @export
#'
#' @examples
#'
#' @import data.table
GroupLines <- function(dt, bufferWidth, timeField = NULL, crs, coordFields = c('EASTING', 'NORTHING'),
                       idField = 'ID', spLines = NULL) {
  # Check for a timeField
  if(is.null(timeField)){
    # Check if spLines is already provided
    if(is.null(spLines)){
      if(is.null(dt)) stop("must provide either spLines or dt")
      # If it isn't, build it
      spLines <- BuildLines(dt, crs, coordFields, idField)
    }
    # Buffer the lines by a provided buffer width
    buffers <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)

    # Find which buffers overlap each other and return
    ovr <- sp::over(spLines, sp::disaggregate(buffers), returnList = T)
    return(data.table::data.table(id = names(ovr), group = unlist(ovr)))
  } else {
    # If timeField is provided, check that spLines is not.
    if(!is.null(spLines)) stop("if providing a spLines, cannot provide a time field")
    # Build and buffer as above, by timeField. Return spatial and unique groups
    dt[, {spLines <- BuildLines(.SD, crs, coordFields, idField)
          buffers <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
          ovr <- sp::over(spLines, sp::disaggregate(buffers), returnList = T)
          list(id = names(ovr), group = unlist(ovr))},
    by = timeField,
    .SDcols = c(coordFields, idField)][, group := .GRP, by = .(spatialGroup, get(timeField))][]
  }
}


# if(is.null(spLines)){
#   if(is.null(dt)) stop("must provide either spLines or dt")
#
#   spLines <- BuildLines(dt, crs, coordFields, idField)
# }
#
# # Buffer those lines by a preassigned buffer width
# buffers <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
#
# # Find which buffers overlap each other and return as a list
# ovr <- sp::over(spLines, sp::disaggregate(buffers), returnList = T)
# return(data.table::data.table(id = names(ovr), group = unlist(ovr)))

# TODO: optional buffer
