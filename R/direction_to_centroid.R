#' Direction to group centroid
#'
#' `direction_to_centroid` calculates the direction of each relocation to
#' the centroid of the spatiotemporal group identified by `group_pts`. The
#' function expects a `data.table` with relocation data appended with a
#' `group` column from `group_pts` and centroid columns from
#' `centroid_group`. Relocation data should be in planar coordinates
#' provided in two columns representing the X and Y coordinates.
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' This function expects a `group` column present generated with the
#' `group_pts` function and centroid coordinate columns generated with the
#' `centroid_group` function. The `coords` and `group` arguments
#' expect the names of columns in `DT` which correspond to the X and Y
#' coordinates and group columns.
#'
#' @inheritParams distance_to_centroid
#' @inheritParams direction_step
#' @inheritParams group_pts
#'
#' @return `direction_to_centroid` returns the input `DT` appended with a
#'   `direction_centroid` column indicating the direction to the group centroid
#'   in radians. A value of NaN is returned when the coordinates of the focal
#'   individual equal the coordinates of the centroid.
#'
#'   A message is returned when `direction_centroid` column already exist
#'   in the input `DT`, because they will be overwritten.
#'
#'   An error is returned if there are any missing values in coordinates for
#'   the focal individual or the group leader, as the underlying direction
#'   function ([lwgeom::st_geod_azimuth()]) does not accept missing values.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @export
#' @family Direction functions
#' @family Centroid functions
#' @seealso [centroid_group], [group_pts], [lwgeom::st_geod_azimuth()]
#' @references
#' See example of using direction to group centroid:
#'  * \doi{doi:10.1016/j.cub.2017.08.004}
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
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '20 minutes')
#'
#' # Spatial grouping with timegroup
#' group_pts(DT, threshold = 5, id = 'ID',
#'           coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Calculate group centroid
#' centroid_group(DT, coords = c('X', 'Y'), group = 'group', na.rm = TRUE)
#'
#' # Calculate direction to group centroid
#' direction_to_centroid(DT, coords = c('X', 'Y'), crs = 32736)
direction_to_centroid <- function(
    DT = NULL,
    coords = NULL,
    crs = NULL,
    geometry = 'geometry') {

  # Due to NSE notes in R CMD check
  direction_centroid <- NULL

  assert_not_null(DT)
  assert_is_data_table(DT)

  out_colname <- 'direction_centroid'
  if (out_colname %in% colnames(DT)) {
    message(out_colname, ' column will be overwritten by this function')
    data.table::set(DT, j = out_colname, value = NULL)
  }

  if (is.null(coords)) {
    if (!is.null(crs)) {
      message('crs argument is ignored when geometry provided')
    }

    centroid_column <- 'centroid'

    assert_are_colnames(DT, geometry)
    assert_col_inherits(DT, geometry, 'sfc_POINT')
    assert_are_colnames(DT, centroid_column, ', did you run centroid_group?')
    assert_col_inherits(DT, centroid_column, 'sfc_POINT')

    DT[, c(out_colname) := calc_direction(
      geometry_a = focal,
      geometry_b = centroid
    ),
    env = list(focal = geometry, centroid = 'centroid')]

  } else {
    assert_are_colnames(DT, coords)
    assert_length(coords, 2)
    assert_col_inherits(DT, coords, 'numeric')

    xcol <- data.table::first(coords)
    ycol <- data.table::last(coords)
    pre <- 'centroid_'
    centroid_xcol <- paste0(pre, xcol)
    centroid_ycol <- paste0(pre, ycol)
    centroid_coords  <- c(centroid_xcol, centroid_ycol)

    assert_are_colnames(DT, centroid_coords, ', did you run centroid_group?')
    assert_col_inherits(DT, centroid_coords, 'numeric')

    if (is.null(crs)) {
      crs <- sf::NA_crs_
    }

    DT[, c(out_colname) := calc_direction(
      x_a = x_focal, y_a = y_focal,
      x_b = x_centroid, y_b = y_centroid,
      crs = crs
    ),
    env = list(x_focal = xcol, y_focal = ycol,
               x_centroid = centroid_xcol, y_centroid = centroid_ycol)]
  }

  return(DT[])
}
