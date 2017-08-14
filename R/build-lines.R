#' Build Lines
#'
#' @inheritParams BuildPts
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

  l <- lapply(seq_along(lst), function(i){
    sp::SpatialLines(list(sp::Lines(sp::Line(cbind(lst[[i]][[coordFields[1]]],
                                                   lst[[i]][[coordFields[2]]])),
                                    names(lst)[[1]])))
  })
  do.call(rbind, l)
}
