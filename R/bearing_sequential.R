#' Calculate absolute sequential bearing
#'
#' Ensure input DT is ordered by datetime or timegroup using
#' eg. setorder(DT, timegroup)
#' @export
bearing_sequential <- function(DT, id = NULL, coords = NULL, projection = NULL) {
  stopifnot(!is.null(id))
  stopifnot(!is.null(coords))
  stopifnot(!is.null(projection))

  if (sf::st_is_longlat(projection)) {
    DT[, bearing := c(
      units::drop_units(
        lwgeom::st_geod_azimuth(sf::st_as_sf(.SD, coords = coords, crs = projection))),
      NA),
      by = c(id)]
  } else {
    DT[, bearing := c(
      units::drop_units(
        lwgeom::st_geod_azimuth(sf::st_transform(
          sf::st_as_sf(.SD, coords = coords, crs = projection),
          crs =  4326))),
      NA),
      by = c(id)]
  }
}
