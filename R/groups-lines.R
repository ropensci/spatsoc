#' Groups From Spatial Lines
#'
#' @param in.dt the 3 column data.table with coordinates (EASTING, NORTHING) and
#'   ID. if non-standard column names are used, provide them with arguments..
#'
#' @return id by spatial group data.table
#' @export
#'
#' @examples
grp.lines <- function(in.dt, crs) {
  # Split up the data.table by collar ID into lists
  lst <- data.table:::split.data.table(in.dt[, list(EASTING, NORTHING)], list(in.dt[, list(ID)]))

  `%do%` <- foreach::`%do%`

  # Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
  sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
    mapview::coords2Lines(as.matrix(c(i$EASTING, i$NORTHING)), ID = id,
                          proj4string = sp::CRS(crs))
  }


  # Buffer those lines by a preassigned buffer width
  buffers <- rgeos::gBuffer(sp.lines, width = buffer.width, byid = FALSE)

  # Find which buffers overlap each other and return as a list
  ovr <- sp::over(sp.lines, sp::disaggregate(buffers), returnList = T)
  dt <- data.table::data.table(id = names(ovr), group = ovr)
}
