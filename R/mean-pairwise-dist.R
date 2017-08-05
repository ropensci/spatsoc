#' Euclidean Pairwise Distance
#'
#' @param locs input data.table with X, Y and ID columns
#' @param time.col time column upon which individuals are compared. recommended
#'   to use a rounded time, see SOME LINK.
#' @param id.col ID column name, if not provided defaults to 'ID'
#' @param east.col X column name, if not provided defaults to 'EASTING'
#' @param north.col Y column name, if not provided defaults to 'NORTHING'
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
mean_pairwise_dist <- function(in.dt, time.col = NULL, id.col = 'ID', east.col = 'EASTING', north.col = 'NORTHING') {
  if(is.null(time.col)) {
    warning('time column not provided - pairwise distance will be computed across all locs')
  }

  # in.dt[, .... , by = time.col]

  names <- in.dt[, get(id.col)]

  dst.mtrx <- sp::spDists(as.matrix(in.dt[ , .(get(east.col), get(north.col))]),
                          longlat = FALSE)

  # this should call another function to flex on the time
  # or can we do by a NULL column?

  # !!!! check for if any names are equal, out warning

  # Output the column means (average pairwise dist) + names
  list(meanDistance = colMeans(dst.mtrx),
             id = names)
}
