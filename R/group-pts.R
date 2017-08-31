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
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' data(locs)
#' groups <- GroupPts(locs, '+init=epsg:4326', 50)
#'
#' groups <- GroupPts(locs, 50, timeField = 'FIX_DATE',
#'         projection = '+proj=utm +zone=21 ellps=WGS84',
#'         idField = 'ID')
#'
#' data(locsPts)
#'
#' groups <- GroupPts(spPts = locsPts)
GroupPts <- function(dt, bufferWidth, timeField = NULL, projection, coordFields = c('EASTING', 'NORTHING'),
                     idField = 'ID', spPts = NULL){
  if(any(!(c(timeField, idField, coordFields) %in% colnames(dt)))){
    stop('some fields provided are not present in data.table provided/colnames(dt)')
  }
  if(is.null(timeField)){
    if(is.null(spPts)){
      if(is.null(dt)) stop("must provide either pts or dt")
      spPts <- BuildPts(dt, projection, coordFields, idField)
    }
    buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
    ovr <- sp::over(spPts, sp::disaggregate(buffers))
    dt <- data.table::data.table(spPts@coords, id = spPts$id, group = ovr)
  } else {
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")
    dt[, {spPts <- BuildPts(.SD, projection, coordFields, idField)
          buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
          ovr <- sp::over(spPts, sp::disaggregate(buffers))
          # c(as.list(as.data.frame(spPts@coords)),
          #   list(id = spPts$id, group = paste(ovr, .GRP, sep = '_')))},
          setNames(c(as.list(as.data.frame(spPts@coords)),
                     list(spPts$id, paste(ovr, .GRP, sep = '_'))),
                   c(coordFields, idField, 'group'))
          },
       by = timeField, .SDcols = c(coordFields, idField)]
  }
}

# TODO: check that drop is not much slower ~drop spatialGroup on output~
# TODO: a[, uniqueN(spatialGroup), by = group]
