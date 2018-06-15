#' Group Points
#'
#' Group points by spatial and temporal overlap.
#'
#' This function finds spatialtemporal groups in input points. The threshold provided must be in the units of the projection of the coordinates. UTM coordinates (recommended) are in meters and threshold = 50 indicates a 50m threshold.
#'
#' @inheritParams BuildPts
#' @param  threshold for grouping points, in the units of the projection
#' @param timegroup (optional) timegroup field in the DT upon which the grouping will be calculated
#' @param groupFields (optional) character string or vector of grouping field(s) upon which the grouping will be calculated
#' @return Input data.table with column 'group' added.
#' @export
#'
#' @examples
#'
#' group_pts(locs, threshold = 5, id = 'ID',
#'          coords = c('X', 'Y'))
#'
#' group_pts(locs, threshold = 5, id = 'ID',
#'          coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' group_pts(locs, threshold = 5, id = 'ID', coords = c('X', 'Y'),
#'          timegroup = 'timegroup', groupFields = 'season')
group_pts <- function(DT = NULL,
                     threshold = NULL,
                     id = NULL,
                     coords = NULL,
                     timegroup = NULL,
                     groupFields = NULL) {
  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(threshold)) {
    stop('threshold required')
  }

  if (threshold <= 0) {
    stop('threshold must be greater than 0')
  }

  if (is.null(id)) {
    stop('ID field required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (any(!(
    c(timegroup, id, coords, groupFields) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(timegroup, id, coords, groupFields),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  if (!is.null(timegroup)) {
    if (any(DT[, sapply(.SD, class), .SDcols = timegroup] %in%
            c('POSIXct', 'POSIXlt', 'Date', 'IDate', 'ITime', 'character'))) {
      warning('timegroup provided is a date/time or character type, did you use group_times?')
    }
    }

  if ('group' %in% colnames(DT)) {
    warning('group column will be overwritten by this function')
    set(DT, j = 'group', value = NULL)
  }

  if (is.null(timegroup) & is.null(groupFields)) {
    groupFields <- NULL
  } else {
    groupFields <- c(groupFields, timegroup)
  }
  # warn if multiple IDs by timegroup/groupFields
  DT[, withinGroup := {
    distMatrix <-
      as.matrix(dist(cbind(
        get(coords[1]), get(coords[2])
      ),
      method = 'euclidean'))
    graphAdj <-
      igraph::graph_from_adjacency_matrix(distMatrix <= threshold)
    igraph::clusters(graphAdj)$membership
  },
  by = groupFields, .SDcols = c(coords, id)]
  DT[, group := .GRP,
     by = c(groupFields, 'withinGroup')]
  set(DT, j = 'withinGroup', value = NULL)
  return(DT[])
}
