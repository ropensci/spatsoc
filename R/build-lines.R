#' Build Lines
#'
#' @inheritParams BuildPts
#'
#' @return SpatialLines for each ID provided
#' @export
#'
#' @import data.table
BuildLines <- function(dt, projection, coordFields = c('EASTING', 'NORTHING'), idField = 'ID') {
  if(any(!(c(timeField, coordFields) %in% colnames(dt)))){
    stop('some fields provided are not present in data.table provided/colnames(dt)')
  }
  # Find any ids with only one loc (rgeos requires at least 2 locs for a line buffer)
  dropRows <- dt[, .(dropped = .N < 2), by = idField]

  # if(dropRows[(dropped), .N] > 0) {
  #   message('some rows dropped, cannot build lines with less than two points')}

  # Split up the data.table by collar ID into lists
  lst <- data.table:::split.data.table(dt[get(idField) %in% dropRows[!(dropped), get(idField)],
                                          ..coordFields],
                                       dt[get(idField) %in% dropRows[!(dropped), get(idField)],
                                          .(get(idField))])
  if(length(lst) == 0){
    return(NULL)
  } else {
    l <- lapply(seq_along(lst), function(i){
      sp::SpatialLines(list(sp::Lines(sp::Line(cbind(lst[[i]][[coordFields[1]]],
                                                     lst[[i]][[coordFields[2]]])),
                                      names(lst)[[i]])),
                       proj4string = sp::CRS(projection))
    })
    do.call(sp::rbind.SpatialLines, l)
  }
}

# TODO: one single warning with count of how many dropped
# TODO: should this be flexible to time?
