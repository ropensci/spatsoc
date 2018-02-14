#' Group Points By Buffer
#'
#' Group points by buffer overlap at equal time.
#'
#' This function uses input spatial points to determine groups in space and
#' time.
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
                           projection, coordFields = c('EASTING', 'NORTHING'),
                           idField = 'ID', spPts = NULL){

  if(!is.null(DT) && any(!(c(time, idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!is.null(DT) & "group" %in% colnames(DT)) warning("`group` column will be overwritten by this function")
  if(is.null(time)){

  if(is.null(time) & is.null(groupFields)){
    distMatrix <- as.matrix(dist(DT[, coordFields, with = FALSE]))
    graphAdj <- igraph::graph_from_adjacency_matrix(distMatrix <= distance)
    group <- igraph::clusters(graphAdj)$membership
    data.table(ID = names(group), group)

  } else {

    byFields <- c(groupFields, time)

    DT[, withinGroup := {
      distMatrix <- as.matrix(dist(.SD[, coordFields, with = FALSE]))
      graphAdj <- igraph::graph_from_adjacency_matrix(distMatrix <= distance)
      igraph::clusters(graphAdj)$membership
    },
    by = byFields, .SDcols = c(coordFields, idField)]
    DT[, groupIG := .GRP, by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
  }
}

