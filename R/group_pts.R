#' Group Points
#'
#' Group points by spatial and temporal overlap.
#'
#' This function finds spatialtemporal groups in input points. The threshold provided must be in the units of the projection of the coordinates. UTM coordinates (recommended) are in meters and threshold = 50 indicates a 50m threshold.
#'
#' @param DT input data.table
#' @param threshold for grouping points, in the units of the projection
#' @param id Character string of ID column name
#' @param coords Character vector of X coordinate and Y coordinate column names
#' @param timegroup (optional) timegroup field in the DT upon which the grouping will be calculated
#' @param splitBy (optional) character string or vector of grouping field(s) upon which the grouping will be calculated
#'
#'
#' @return Input data.table with column 'group' added.
#' @export
#'
#' @importFrom stats dist
#'
#' @examples
#' library(data.table)
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#' DT[, datetime := as.POSIXct(datetime,
#'                             tz = 'UTC')]
#'
#' # Spatial grouping without timegroup
#' group_pts(DT, threshold = 5, id = 'ID',
#'          coords = c('X', 'Y'))
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '20 minutes')
#' # Spatial grouping with timegroup
#' group_pts(DT, threshold = 5, id = 'ID',
#'           coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Spatial grouping with timegroup and splitBy on population
#' group_pts(DT, threshold = 5, id = 'ID', coords = c('X', 'Y'),
#'          timegroup = 'timegroup', splitBy = 'population')
group_pts <- function(DT = NULL,
                     threshold = NULL,
                     id = NULL,
                     coords = NULL,
                     timegroup = NULL,
                     splitBy = NULL) {
  # due to NSE notes in R CMD check
  N <- withinGroup <- ..id <- ..coords <- group <- NULL

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
    c(timegroup, id, coords, splitBy) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(timegroup, id, coords, splitBy),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  if (!is.null(timegroup)) {
    if (any(unlist(lapply(DT[, .SD, .SDcols = timegroup], class)) %in%
            c('POSIXct', 'POSIXlt', 'Date', 'IDate', 'ITime', 'character'))) {
      warning(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'timegroup provided is a date/time
          or character type, did you use group_times?'
        )
      )
    }
  }

  if ('group' %in% colnames(DT)) {
    warning('group column will be overwritten by this function')
    set(DT, j = 'group', value = NULL)
  }

  if (is.null(timegroup) && is.null(splitBy)) {
    splitBy <- NULL
  } else {
    splitBy <- c(splitBy, timegroup)
    if (DT[, .N, by = c(id, splitBy, timegroup)][N > 1, sum(N)] != 0) {
      warning(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'found duplicate id in a
          timegroup and/or splitBy -
          does your group_times threshold match the fix rate?'
        )
      )
    }
  }

  DT[, withinGroup := {
    distMatrix <-
      as.matrix(dist(cbind(
        get(..coords[1]), get(..coords[2])
      ),
      method = 'euclidean'))
    graphAdj <-
      igraph::graph_from_adjacency_matrix(distMatrix <= threshold)
    igraph::clusters(graphAdj)$membership
  },
  by = splitBy, .SDcols = c(coords, id)]
  DT[, group := .GRP,
     by = c(splitBy, 'withinGroup')]
  set(DT, j = 'withinGroup', value = NULL)
  return(DT[])
}
