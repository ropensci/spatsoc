#' Direction step
#'
#' `direction_step` calculates the direction of movement steps in radians.
#' The function expects a `data.table` with relocation data and individual
#' identifiers. Relocation data should be in two columns representing the X and
#' Y coordinates. Note the order of rows is not modified by this function and
#' therefore users must be cautious to set it explicitly. See example for one
#' approach to setting order of rows using a datetime field.
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' The `id`, `coords`, and optional `splitBy` arguments expect
#' the names of a column in `DT` which correspond to the individual
#' identifier, X and Y coordinates, and additional grouping columns.
#'
#' The `projection` argument expects a character string or numeric defining
#' the coordinate reference system to be passed to [sf::st_crs]. For example,
#' for UTM zone 36S (EPSG 32736), the projection argument is
#' `projection = "EPSG:32736"` or `projection = 32736`. See
#' <https://spatialreference.org> for a list of EPSG codes.
#'
#' The `splitBy` argument offers further control over grouping. If within
#' your `DT`, you have distinct sampling periods for each individual, you
#' can provide the column name(s) which identify them to `splitBy`. The
#' direction calculation by `direction_step` will only consider rows within
#' each `id` and `splitBy` subgroup.
#'
#' @return `direction_step` returns the input `DT` appended with
#'  a direction column with units set to radians using the `units`
#'  package.
#'
#'   This column represents the azimuth between the sequence of points for
#'   each individual computed using `lwgeom::st_geod_azimuth`. Note, the
#'   order of points is not modified by this function and therefore it is
#'   crucial the user sets the order of rows to their specific question
#'   before using `direction_step`. In addition, the direction column
#'   will include an `NA` value for the last point in each sequence of
#'   points since there is no future point to calculate a direction to.
#'
#'   A message is returned when a direction column are already exists in
#'   the input `DT`, because it will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @inheritParams group_pts
#' @inheritParams build_polys
#'
#' @family Direction functions
#' @seealso [amt::direction_abs()], [geosphere::bearing()]
#' @export
#'
#' @examples
#' # Load data.table
#' library(data.table)
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
#' # Example result for East, North, West, South steps
#' example <- data.table(
#'   X = c(0, 5, 5, 0, 0),
#'   Y = c(0, 0, 5, 5, 0),
#'   step = c('E', 'N', 'W', 'S', NA),
#'   ID = 'A'
#' )
#'
#' direction_step(example, 'ID', c('X', 'Y'), projection = 4326)
#' example[, .(step, direction, units::set_units(direction, 'degree'))]
direction_step <- function(
    DT = NULL,
    id = NULL,
    coords = NULL,
    projection = NULL,
    splitBy = NULL) {

  # due to NSE notes in R CMD check
  direction <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(id)) {
    stop('id column name required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  check_cols <- c(id, coords, splitBy)
  if (!all((check_cols %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        check_cols,
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (!all((DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  if ('direction' %in% colnames(DT)) {
    message('direction column will be overwritten by this function')
    data.table::set(DT, j = 'direction', value = NULL)
  }

  if (is.null(projection)) {
    stop('projection required')
  }

  if (sf::st_is_longlat(projection)) {
    DT[, direction := c(
      lwgeom::st_geod_azimuth(
        sf::st_as_sf(.SD, coords = coords, crs = projection)),
      units::set_units(NA, 'rad')),
      by = c(id, splitBy)]
  } else if (!sf::st_is_longlat(projection)) {
    DT[, direction := c(
      lwgeom::st_geod_azimuth(
        sf::st_transform(
          sf::st_as_sf(.SD, coords = coords, crs = projection),
          crs = 4326)
        ),
      units::set_units(NA, 'rad')),
      by = c(id, splitBy)]
  }
}
