#' Build Lines
#'
#' @inheritParams build_pts
#'
#' @return SpatialLines for each ID provided
#' @export
#'
#' @examples
#'
#' @import data.table
build_lines <- function(dt, crs, coord.fields = c('EASTING', 'NORTHING'), id.field = 'ID') {
  # Split up the data.table by collar ID into lists
  lst <- data.table:::split.data.table(dt[, ..coord.fields],
                                       dt[, .(get(id.field))])

  `%do%` <- foreach::`%do%`

  # Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
  sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
    mapview::coords2Lines(matrix(c(i[['EASTING']], i[['NORTHING']]), ncol = 2),
                          ID = id,
                          proj4string = sp::CRS(crs))
  }
}


# TODO: check warnings..
