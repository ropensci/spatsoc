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
    mapview::coords2Lines(matrix(c(i[[coordFields[1]]], i[[coordFields[2]]]), ncol = 2),
                          ID = id, data = data.frame(id), match.ID = FALSE,
                          proj4string = sp::CRS(crs))
  }
}


# TODO: check warnings..
