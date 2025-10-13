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
#' @inheritParams group_pts
#'
#' @return `direction_to_centroid` returns the input `DT` appended
#'   with a `direction_centroid` column indicating the direction to group
#'   centroid in radians. The direction is measured in radians in the range
#'   of 0 to 2 * pi from the positive x-axis.
#'
#'   A message is returned when `direction_centroid` column already exist
#'   in the input `DT`, because they will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @export
#' @family Direction functions
#' @family Centroid functions
#' @seealso [centroid_group], [group_pts]
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
#' direction_to_centroid(DT, coords = c('X', 'Y'))
direction_to_centroid <- function(
    DT = NULL,
    coords = NULL) {

  # Due to NSE notes in R CMD check
  direction_centroid <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  xcol <- data.table::first(coords)
  ycol <- data.table::last(coords)
  pre <- 'centroid_'
  centroid_xcol <- paste0(pre, xcol)
  centroid_ycol <- paste0(pre, ycol)
  centroid_coords  <- c(centroid_xcol, centroid_ycol)

  if (!all(coords %in% colnames(DT))) {
    stop(paste0(
      as.character(paste(setdiff(
        coords,
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (!all(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = c(coords)])) {
    stop('coords must be numeric')
  }

  if (!all(centroid_coords %in% colnames(DT))) {
    stop(paste0(
      as.character(paste(setdiff(
        centroid_coords,
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in DT, did you run centroid_group?'
    ))
  }

  if (!all(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = c(centroid_coords)])) {
    stop('centroid coords must be numeric')
  }

  if ('direction_centroid' %in% colnames(DT)) {
    message('direction_centroid column will be overwritten by this function')
    data.table::set(DT, j = 'direction_centroid', value = NULL)
  }

  DT[, direction_centroid := fifelse(
    .SD[[xcol]] == .SD[[centroid_xcol]] &
      .SD[[ycol]] == .SD[[centroid_ycol]],
    units::as_units(NaN, 'rad'),
    units::as_units(
      atan2(.SD[[centroid_ycol]] - .SD[[ycol]],
            (.SD[[centroid_xcol]] - .SD[[xcol]])),
      'rad'
    )
  )]
  DT[direction_centroid < units::as_units(0, 'rad'),
     direction_centroid := direction_centroid + units::as_units(2 * pi, 'rad')]

  return(DT[])
}
