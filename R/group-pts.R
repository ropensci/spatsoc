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
#' @param timeField (optional) time field in the DT upon which the spatial grouping will be
#'   calculated
#' @param groupFields (optional) grouping field(s) to be (optionally) combined with timeField, either character or list of strings
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
GroupPts <- function(DT, bufferWidth, timeField = NULL, groupFields = NULL,
                     projection, coordFields = c('EASTING', 'NORTHING'),
                     idField = 'ID', spPts = NULL){

  if(!is.null(DT) && any(!(c(timeField, idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!is.null(DT) & "group" %in% colnames(DT)) warning("`group` column will be overwritten by this function")
  if(is.null(timeField)){
    if(is.null(spPts)){
      if(is.null(DT)){
        stop("must provide either pts or DT")
      }
      spPts <- BuildPts(DT, projection, coordFields, idField)
    }
    buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
    DT[, group := sp::over(spPts, sp::disaggregate(buffers))][]
  } else {
    if(is.null(groupFields)) byFields <- timeField else byFields <- c(groupFields, timeField)
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")

    ovrDT <- DT[, {spPts <- BuildPts(.SD, projection, coordFields, idField)
    buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
    ovr <- sp::over(spPts, sp::disaggregate(buffers))
    data.table::setnames(
      data.table::data.table(get(idField),
                             unlist(ovr)),
      c(idField, 'withinGroup'))
    },
    by = byFields, .SDcols = c(coordFields, idField)]

    DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
    DT[, group := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
  }
}

#' @export
GroupPtsSF <- function(DT, bufferWidth, timeField = NULL, groupFields = NULL,
                       projection, coordFields = c('EASTING', 'NORTHING'),
                       idField = 'ID', spPts = NULL){

  if(!is.null(DT) && any(!(c(timeField, idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!is.null(DT) & "group" %in% colnames(DT)) warning("`group` column will be overwritten by this function")
  if(is.null(timeField)){
    if(is.null(spPts)){
      if(is.null(DT)){
        stop("must provide either pts or DT")
      }
      pts <- sf::st_as_sf(DT, coords = c('EASTING', 'NORTHING'))
      sf::st_crs(pts) <- projection
    }
    bufs <- sf::st_buffer(pts, bufferWidth)
    un <- sf::st_cast(sf::st_union(bufs), 'POLYGON')
    DT[, group := unlist(sf::st_intersects(pts, un), FALSE, FALSE)][]
  } else {
    if(is.null(groupFields)) byFields <- timeField else byFields <- c(groupFields, timeField)
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")

    DT[, withinGroup := {
      pts <- sf::st_as_sf(.SD, coords = coordFields)
      sf::st_crs(pts) <- projection
      bufs <- sf::st_buffer(pts, 50)
      un <- sf::st_cast(sf::st_union(bufs), 'POLYGON')
      unlist(sf::st_intersects(pts, un), FALSE, FALSE)
    },
    by = byFields, .SDcols = c(coordFields, idField)]

    # DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
    DT[, groupSF := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
  }
}

#' @export
GroupPtsIGRAPH <- function(DT, bufferWidth, timeField = NULL, groupFields = NULL,
                           projection, coordFields = c('EASTING', 'NORTHING'),
                           idField = 'ID', spPts = NULL){

  if(!is.null(DT) && any(!(c(timeField, idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!is.null(DT) & "group" %in% colnames(DT)) warning("`group` column will be overwritten by this function")
  if(is.null(timeField)){
    distMatrix <- as.matrix(dist(DT[, coordFields, with = FALSE]))
    graphAdj <- igraph::graph_from_adjacency_matrix(distMatrix <= bufferWidth)
    clstrs <- igraph::clusters(graphAdj)$membership
    data.table(ID = names(clstrs), clstrs)

  } else {
    if(is.null(groupFields)) byFields <- timeField else byFields <- c(groupFields, timeField)
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")

    DT[, withinGroup := {
      distMatrix <- as.matrix(dist(.SD[, coordFields, with = FALSE]))
      graphAdj <- igraph::graph_from_adjacency_matrix(distMatrix <= bufferWidth)
      igraph::clusters(graphAdj)$membership
      # clsts
      # data.table::setnames(
      # data.table::data.table(get(idField), clstrs)
      # c(idField, 'withinGroup'))
    },
    by = byFields, .SDcols = c(coordFields, idField)]
    # ovrDT
    # DT[ovrDT, withinGroup := withinGroup, on = c(idField, byFields)]
    DT[, groupIG := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
  }
}

