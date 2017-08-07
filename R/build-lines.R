#' Build Lines
#'
#' @inheritParams BuildLines
#'
#' @return SpatialLines for each ID provided
#' @export
#'
#' @examples
#'
#' @import data.table
BuildLines <- function(dt, crs, coordFields = c('EASTING', 'NORTHING'), idField = 'ID') {
  # Split up the data.table by collar ID into lists
  lst <- data.table:::split.data.table(dt[, ..coordFields],
                                       dt[, .(get(idField))])

  `%do%` <- foreach::`%do%`

  # Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
  sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
    mapview::coords2Lines(matrix(c(i[['EASTING']], i[['NORTHING']]), ncol = 2),
                          ID = id,
                          proj4string = sp::CRS(crs))
  }
}


# TODO: check warnings..
