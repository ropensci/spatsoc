calc_direction <- function(
    geometry_a, geometry_b,
    x_a, y_a,
    x_b, y_b,
    crs) {
  if (!missing(geometry_a) && missing(x_a) && missing(y_a)) {
    if (!missing(geometry_b)) {
      lwgeom::st_geod_azimuth(geometry_a, geometry_b)
    } else {
      lwgeom::st_geod_azimuth(geometry_a)
    }
  } else if (missing(geometry_a) && !missing(x_a) && !missing(y_a)) {
    if (!missing(x_b) && !missing(y_b)) {
      lwgeom::st_geod_azimuth(
        x = sf::st_as_sf(cbind(x_a, y_a), crs = crs),
        y = sf::st_as_sf(cbind(x_b, y_b), crs = crs)
      )
    } else {
      lwgeom::st_geod_azimuth(
        x = sf::st_as_sf(cbind(x_a, y_a), crs = crs)
      )
    }
  }
}
