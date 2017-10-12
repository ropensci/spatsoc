#' Groups From Spatial Lines
#'
#' @inheritParams BuildPts
#' @inheritParams GroupPts
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
GroupLines <- function(DT, bufferWidth = 0, timeField = NULL,
                       projection, coordFields = c('EASTING', 'NORTHING'),
                       idField = 'ID', spLines = NULL) {
  if(any(!(c(idField, timeField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  # Check for a timeField
  if(is.null(timeField)){
    # Check if spLines is already provided
    if(is.null(spLines)){
      if(is.null(DT)) stop("must provide either spLines or DT")
      # If it isn't, build it
      spLines <- BuildLines(DT, projection, coordFields, idField)
    }
    if(bufferWidth == 0) {
      merged <- rgeos::gBuffer(spLines, width = 0.0001, byid = F)
    } else {
      merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
    }
    # Find which buffers overlap each other and return
    ovr <- sp::over(spLines, sp::disaggregate(merged), returnList = T)
    data.table::setnames(data.table::data.table(names(ovr),
                                                unlist(ovr)),
                         c(idField, 'group'))
  } else {
    # If timeField is provided, check that spLines is not.
    if(!is.null(spLines)) {
      stop("if providing a spLines, cannot provide a time field")
    }
    # Build and buffer as above, by timeField. Return spatial and unique groups
    DT[, {spLines <- BuildLines(.SD, projection, coordFields, idField)
          if(is.null(spLines)) {
            message('some rows are dropped - unable to build lines with <2 locs')
          } else {
            if(bufferWidth == 0) {
              merged <- rgeos::gBuffer(spLines, width = 0.0001, byid = FALSE)
            } else {
              merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
            }
            ovr <- sp::over(spLines, sp::disaggregate(merged), returnList = TRUE)
            setNames(list(names(ovr), paste(ovr, .GRP, sep = '_')),
                     c(idField, 'group'))
            }
          },
       by = timeField, .SDcols = c(coordFields, idField)]
  }
}
# TODO: find alternative to 0.0001 buffer
# TODO: adjust output names on line 46
