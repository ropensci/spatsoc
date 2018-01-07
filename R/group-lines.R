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
GroupLines <- function(DT, bufferWidth = 0, timeField = NULL, groupFields = NULL,
                       projection, coordFields = c('EASTING', 'NORTHING'),
                       idField = 'ID', spLines = NULL) {
  if(!is.null(DT) && any(!(c(idField, timeField, coordFields) %in% colnames(DT)))){
    print(which(!(c(idField, timeField, coordFields) %in% colnames(DT))))
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!is.null(DT) & "group" %in% colnames(DT)) warning("`group` column will be overwritten by this function")


  if(is.null(timeField)){
    if(is.null(spLines)){
      if(is.null(DT)) stop("must provide either spLines or DT")
      spLines <- BuildLines(DT, projection, coordFields, idField)
    }
    if(bufferWidth == 0) {
      merged <- rgeos::gBuffer(spLines, width = 0.0001, byid = F)
    } else {
      merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
    }
    ovr <- sp::over(spLines, sp::disaggregate(merged), returnList = T)
    ovrDT <- data.table::setnames(
      data.table::data.table(names(ovr),
                             unlist(ovr)),
      c(idField, 'group'))
    DT[ovrDT, group := group, on = idField]
  } else {
    if(!is.null(spLines)) {
      stop("if providing a spLines, cannot provide a time field")
    }

    if(is.null(groupFields)) byFields <- timeField else byFields <- c(groupField, timeField)

    DT[, {spLines <- BuildLines(.SD, projection, coordFields, idField)
          if(is.null(spLines)) {
            message('some rows are dropped - unable to build lines with <3 locs')
          } else {
            if(bufferWidth == 0) {
              merged <- rgeos::gBuffer(spLines, width = 0.0001, byid = FALSE)
            } else {
              merged <- rgeos::gBuffer(spLines, width = bufferWidth, byid = FALSE)
            }
            ovr <- sp::over(spLines, sp::disaggregate(merged), returnList = TRUE)
            data.table::setnames(
              data.table::data.table(names(ovr),
                                     paste(ovr, .GRP, sep = '_')),
              c(idField, 'group'))
          }
    }, by = byFields, .SDcols = c(coordFields, idField)]
  }
}
# TODO: find alternative to 0.0001 buffer
