#' Calculate direction at each step
#'
#' \code{direction_step} calculates the direction of movement steps. The
#' function accepts a \code{data.table} with relocation data and individual
#' identifiers. Relocation data should be in two columns representing the X and
#' Y coordinates. Note the order of rows is not modified by this function and
#' therefore users must be cautious to set it explictly. See example for one
#' approach to setting order of rows using a datetime field.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' The \code{id}, \code{coords}, and optional \code{splitBy} arguments expect
#' the names of a column in \code{DT} which correspond to the individual
#' identifier, X and Y coordinates, and additional grouping columns.
#'
#' The \code{projection} argument expects a character string or numeric defining
#' the coordinate reference system to be passed to [sf::st_crs]. For example,
#' for UTM zone 36S (EPSG 32736), the projection argument is
#' \code{projection = "EPSG:32736"} or \code{projection = 32736}. See
#' <https://spatialreference.org> for #' a list of EPSG codes.
#'
#' The \code{splitBy} argument offers further control over grouping. If within
#' your \code{DT}, you have distinct sampling periods for each individual, you
#' can provide the column name(s) which identify them to \code{splitBy}. The
#' direction calculation by \code{direction_step} will only consider rows within
#' each \code{id} and \code{splitBy} subgroup.
#'
#' @return \code{direction_step} returns the input \code{DT} appended with
#'  a direction column.
#'
#'   This column represents the azimuth between the sequence of points for
#'   each individual computed using \code{lwgeom::st_geod_azimuth}. Note, the
#'   order of points is not modified by this function and therefore it is
#'   crucial the user sets the order of rows to their specific question
#'   before using \code{direction_step}.
#'
#'   A message is returned when a direction column are already exists in
#'   the input \code{DT}, because it will be overwritten.
#'
#' @inheritParams group_pts
#' @inheritParams build_polys
#'
#' @family Direction functions
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
direction_step <- function(
    DT = NULL,
    id = NULL,
    coords = NULL,
    projection = NULL,
    splitBy = NULL) {

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
  if (any(!(check_cols %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        check_cols,
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
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
      units::drop_units(
        lwgeom::st_geod_azimuth(
          sf::st_as_sf(.SD, coords = coords, crs = projection))
        ),
      NA),
      by = c(id, splitBy)]
  } else if (!sf::st_is_longlat(projection)) {
    DT[, direction := c(
      units::drop_units(
        lwgeom::st_geod_azimuth(
          sf::st_transform(
            sf::st_as_sf(.SD, coords = coords, crs = projection),
            crs = 4326)
          )
        ),
      NA),
      by = c(id, splitBy)]
  } else {
    stop('projection not recognized, please see sf::st_crs')
  }
}
