#' Group Points By Buffer
#'
#' Group points by spatial distance and temporal overlap.
#'
#' This function finds spatialtemporal groups in input points.
#'
#' @inheritParams BuildPts
#' @param distance The threshold distance for grouping points. The distance must be in the units of the projection.
#' @param time (optional) time field in the DT upon which the spatial grouping will be calculated.
#' @param groupFields (optional) grouping field(s) to be combined with the time field, either character or list of strings.
#' @return Input DT with column "group" added.
#' @export
#'
#' @examples
#'
#' GroupPts(locs, distance = 50)
#'
#' GroupPts(locs, distance = 50, time = 'datetime')
#'
#' GroupPts(locs, distance = 50, time = 'timegroup')
#'
#' GroupPts(locs, distance = 50, time = 'timegroup', groupFields = 'season')
GroupPts <- function(DT, distance, time = NULL, groupFields = NULL,
                     coordFields = c('EASTING', 'NORTHING'), idField = NULL){

  if(is.null(DT)) stop('input DT required')

  if(is.null(distance)) stop('distance threshold required')

  if(is.null(idField)) stop('ID field required')

  if(any(!(c(time, idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in input DT')
  }

  if(!is.null(DT) & "group" %in% colnames(DT)){
    warning("`group` column will be overwritten by this function")
    DT[, group := NULL]
  }

  if(!all(sapply(DT[, coordFields, with = FALSE], is.numeric))) stop('ensure that input coordFields are numeric')

  if(is.null(time) & is.null(groupFields)){
    distMatrix <- as.matrix(dist(DT[, ..coordFields]))
    graphAdj <- igraph::graph_from_adjacency_matrix(distMatrix <= distance)
    group <- igraph::clusters(graphAdj)$membership
    data.table(ID = names(group), group)

  } else {

    byFields <- c(groupFields, time)

    DT[, withinGroup := {
      distMatrix <- as.matrix(dist(.SD[, ..coordFields]))
      graphAdj <- igraph::graph_from_adjacency_matrix(distMatrix <= distance)
      igraph::clusters(graphAdj)$membership
    },
    by = byFields, .SDcols = c(coordFields, idField)]
    DT[, group := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
  }
}

