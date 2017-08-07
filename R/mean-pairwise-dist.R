#' Euclidean Pairwise Distance
#'
#' @inheritParams BuildPts
#' @param timeField time column upon which individuals are compared. recommended
#'   to use a rounded time, see SOME LINK.
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
PairwiseDist <- function(dt, timeField, coordFields = c('EASTING', 'NORTHING'), idField = 'ID') {
  if(is.null(timeField)) {
    warning('time column not provided - pairwise distance will be computed across all locs')
  }

  # dt[, .... , by = time.col]

  names <- dt[, get(idField)]

  distMatrix <- sp::spDists(as.matrix(dt[ , ..coordFields]),
                          longlat = FALSE)

  # this should call another function to flex on the time
  # or can we do by a NULL column?

  # !!!! check for if any names are equal, out warning

  # Output the column means (average pairwise dist) + names
  data.table(meanDistance = colMeans(distMatrix),
             id = names)
}
