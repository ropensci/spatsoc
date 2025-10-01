calc_direction <- function(
    geometry_a, geometry_b,
    x_a, y_a,
    x_b, y_b) {
  if (!missing(geometry_a) & missing(x_a) & missing(y_a)) {
    if (!missing(geometry_b)) {
      lwgeom::st_geod_azimuth(geometry_a, geometry_b)
    } else {
      lwgeom::st_geod_azimuth(geometry_a)
    }
  } else if (missing(geometry_a) & !missing(x_a) & !missing(y_a)) {
    if (!missing(x_b) & !missing(y_b)) {
      atan2(x_b - x_b, y_b - y_a)
    } else {
      atan2(diff(x_a), diff(y_a))
    }
  }
}
