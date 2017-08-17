#' Euclidean Pairwise Distance
#'
#' @inheritParams BuildPts
#' @param timeField time column upon which individuals are compared. recommended
#'   to use a rounded time, see SOME LINK.
#'
#' @return mean pairwise distance data.table by ID and (optional) time field
#' @export
#'
#' @import data.table
PairwiseDist <- function(dt, timeField, coordFields = c('EASTING', 'NORTHING'), idField = 'ID') {
  if(is.null(timeField)) {
    warning('time column not provided - pairwise distance will be computed across all locs')

    names <- dt[, get(idField)]

    distMatrix <- sp::spDists(as.matrix(dt[ , ..coordFields]),
                              longlat = FALSE)

    # Output the column means (average pairwise dist) + names
    data.table(meanDistance = colMeans(distMatrix),
               id = names)
  } else {
    dt[, {names <- .SD[, get(idField)]
          distMatrix <- sp::spDists(as.matrix(.SD[ , ..coordFields]),
                                    longlat = FALSE)
          list(meanDistance = colMeans(distMatrix),
               id = names)},
       by = timeField]
  }
}

# TODO: check for if any names are equal, output warning
