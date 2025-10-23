#' Calculate direction
#'
#' **Internal function** - not developed to be used outside of spatsoc functions
#'
#' Calculate direction using [lwgeom::st_geod_azimuth()] between one of:
#' - the sequence of points in geometry_a
#' - the sequence of points in x_a, y_a
#' - the pairwise points in geometry_a and geometry_b
#' - the pairwise points in x_a, y_a and x_b, y_b
#'
#' Requirements:
#' - matching length between a and b objects if b provided
#' - crs is longlat, check with [sf::st_is_longlat()]
#'
#' @param geometry_a,geometry_b sfc (simple feature geometry list column) from [get_geometry()]
#' @param x_a,x_b X coordinate column, numeric
#' @param y_a,y_b Y coordinate column, numeric
#' @param crs crs for x_a, y_a (and if provided, x_b, y_b) coordinates,
#' ignored for geometry_a and geometry_b arguments
#'
#' @returns
#'
#' Direction in units of radians, in range of pi, -pi where North = 0.
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Example result for East, North, West, South directions
#' example <- data.table(
#'   X = c(0, 5, 5, 0, 0),
#'   Y = c(0, 0, 5, 5, 0)
#' )
#' # E, N, W, S
#' example[, calc_direction(x_a = X, y_a = Y, crs = 4326)]
calc_direction <- function(
    geometry_a, geometry_b,
    x_a, y_a,
    x_b, y_b,
    crs) {
  if (!missing(geometry_a) && missing(x_a) && missing(y_a)
      && missing(x_b) && missing(y_b)) {
    if (!missing(geometry_b)) {
      lwgeom::st_geod_azimuth(geometry_a, geometry_b)
    } else {
      lwgeom::st_geod_azimuth(geometry_a)
    }
  } else if (missing(geometry_a) && !missing(x_a) && !missing(y_a)) {
    if (!missing(x_b) && !missing(y_b)) {
      lwgeom::st_geod_azimuth(
        x = sf::st_as_sf(data.frame(x_a, y_a), crs = crs, coords = seq.int(2)),
        y = sf::st_as_sf(data.frame(x_b, y_b), crs = crs, coords = seq.int(2))
      )
    } else {
      lwgeom::st_geod_azimuth(
        x = sf::st_as_sf(data.frame(x_a, y_a), crs = crs, coords = seq.int(2))
      )
    }
  } else {
    rlang::abort(c(
      'arguments incorrectly provided, use one of the following combinations:',
      '1. geometry_a',
      '2. geometry_a and geometry_b',
      '3. x_a, y_a',
      '4. x_a, y_a, and x_b, y_b'
    ))
  }
}
