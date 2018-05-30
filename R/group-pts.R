#' Group Points By Buffer
#'
#' Group points by spatial distance and temporal overlap.
#'
#' This function finds spatialtemporal groups in input points.
#'
#' @inheritParams BuildPts
#' @param distance The threshold distance for grouping points. The distance must be in the units of the projection.
#' @param timeGroup (optional) time group field in the DT upon which the spatial grouping will be calculated.
#' @param groupFields (optional) grouping field(s) to be combined with the timeGroup field, either character or list of strings.
#' @return Input DT with column "group" added.
#' @export
#'
#' @examples
#'
#' GroupPts(locs, distance = 50)
#'
#' GroupPts(locs, distance = 50, timeGroup = 'datetime')
#'
#' GroupPts(locs, distance = 50, timeGroup = 'timegroup')
#'
#' GroupPts(locs, distance = 50, timeGroup = 'timegroup', groupFields = 'season')
GroupPts <- function(DT,
                     distance = NULL,
                     timeGroup = NULL,
                     groupFields = NULL,
                     coordFields = NULL,
                     idField = NULL) {
  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(distance)) {
    stop('distance threshold required')
  }

  if (distance <= 0) {
    stop('distance must be greater than 0')
  }

  if (is.null(idField)) {
    stop('ID field required')
  }

  if (length(coordFields) != 2) {
    stop('coordFields requires a vector of column names for coordinates X and Y')
  }

  if (any(!(c(timeGroup, idField, coordFields, groupFields) %in% colnames(DT)))) {
    stop('some fields provided are not present in input DT')
  }

  if (any(!(DT[, lapply(.SD, is.numeric), .SDcols = coordFields]))) {
    stop('coordFields must be numeric')
  }

  if ("group" %in% colnames(DT)) {
    warning("`group` column will be overwritten by this function")
    DT[, group := NULL]
  }

  if (!all(sapply(DT[, coordFields, with = FALSE], is.numeric)))
    stop('ensure that input coordFields are numeric')

  if (is.null(timeGroup) & is.null(groupFields)) {
    distMatrix <- as.matrix(dist(DT[, ..coordFields]))
    graphAdj <-
      igraph::graph_from_adjacency_matrix(distMatrix < distance)
    group <- igraph::clusters(graphAdj)$membership

    return(data.table(ID = names(group), group))

  } else {
    byFields <- c(groupFields, timeGroup)
    DT[, withinGroup := {
      distMatrix <-
        as.matrix(dist(
          cbind(get(coordFields[1]), get(coordFields[2])),
          method = "euclidean"))
      graphAdj <-
        igraph::graph_from_adjacency_matrix(distMatrix <= distance)
      igraph::clusters(graphAdj)$membership
    },
    by = byFields, .SDcols = c(coordFields, idField)]
    DT[, group := .GRP,
       by = c(byFields, 'withinGroup')][, withinGroup := NULL][]
  }
}
