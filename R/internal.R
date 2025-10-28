#' Difference of two angles measured in radians
#'
#' Internal function
#'
#' @param x angle in radians
#' @param y angle in radians
#' @param signed boolean if signed difference should be returned, default FALSE
#' @param return_units return difference with units = 'rad'
#'
#' @return Difference between x and y in radians. If signed is TRUE, the signed
#'   difference is returned. If signed is FALSE, the absolute difference is
#'   returned. Note: The difference is the smallest difference, eg. the
#'   difference between 2 rad and -2.5 rad is 1.78.
#' @references adapted from https://stackoverflow.com/a/7869457
#' @keywords internal
#' @examples
#' # Load data.table, units
#' library(data.table)
#' library(units)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Set order using data.table::setorder
#' setorder(DT, datetime)
#'
#' # Calculate direction
#' direction_step(
#'   DT = DT,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   projection = 32736
#' )
#'
#' # Differences
#' spatsoc:::diff_rad(DT[1, direction], DT[2, direction])
#'
#' # Note smallest difference returned
#' spatsoc:::diff_rad(as_units(2, 'rad'), as_units(-2.5, 'rad'))
 diff_rad <- function(x, y,  signed = FALSE, return_units = FALSE) {
  if (!inherits(x, 'units') || units(x)$numerator != 'rad') {
    stop('units(x) is not radians')
  }
  if (!inherits(y, 'units') || units(y)$numerator != 'rad') {
    stop('units(y) is not radians')
  }

  d <- units::drop_units(y) - units::drop_units(x)
  d <- ((d + pi) %% (2 * pi)) - pi

  if (signed) {
    out <- d
  } else {
    out <- abs(d)
  }

  if (return_units) {
    return(units::as_units(out, 'rad'))
  } else {
    return(out)
  }
}

#' Calculate centroid
#'
#' **Internal function** - not developed to be used outside of spatsoc functions
#'
#' Calculate centroid using [sf::st_centroid()] for one of:
#' - geometry
#' - the points in x, y
#' - the pairwise points in geometry_a and geometry_b
#' - the pairwise points in x_a, y_a and x_b, y_b
#' @param geometry sfc (simple feature geometry list column) from [get_geometry()]
#' @param x X coordinate column, numeric
#' @param y Y coordinate column, numeric
#' @param crs crs for x, y coordinates, ignored for geometry argument
#'
#' @returns
#'
#' Centroid of the geometry or coordinates in x,y provided
#'
#' @keywords internal
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{
#' data.table::setDTthreads(1)
#' }
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' DT[, spatsoc:::calc_centroid(x = X, y = Y, crs = 32736)]
calc_centroid <- function(geometry, x, y, crs) {
  if (!missing(geometry) && missing(x) && missing(y)) {
    sf::st_centroid(sf::st_combine(geometry))
  } else if (missing(geometry) && !missing(x) && !missing(y)) {
    sf::st_centroid(sf::st_combine(
      sf::st_as_sf(
        data.frame(x, y),
        crs = crs,
        coords = seq.int(2),
        na.fail = FALSE
      )
    ))
  } else {
    rlang::abort(c(
      'arguments incorrectly provided, use one of the following combinations:',
      '1. geometry',
      '2. x, y'
    ))
  }
}
