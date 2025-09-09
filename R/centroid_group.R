#' Group centroid
#'
#' `centroid_group` calculates the centroid (mean location) of all
#' individuals in each spatiotemporal group identified by `group_pts`. The
#' function expects a `data.table` with relocation data appended with a
#' `group` column from `group_pts`. Relocation data should be in two
#' columns representing the X and Y coordinates.
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' The `coords` and `group` arguments expect the names of a column in
#' `DT` which correspond to the X and Y coordinates and group columns. The
#' `na.rm` argument is passed to the `mean` function to control if NA
#' values are removed before calculation.
#'
#' @param DT input data.table with group column generated with `group_pts`
#' @inheritParams group_pts
#' @param group Character string of group column
#' @param na.rm if NAs should be removed in calculating mean location,
#' see `mean`
#'
#' @return `centroid_group` returns the input `DT` appended with
#'  centroid columns for the X and Y coordinate columns.
#'
#'   These columns represents the centroid coordinate columns.
#'   The naming of these columns will correspond to the provided coordinate
#'   column names prefixed with "centroid_".
#'
#'   A message is returned when centroid columns are already exists in
#'   the input `DT`, because they will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @export
#' @seealso \code{\link{group_pts}}
#' @family Centroid functions
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
centroid_group <- function(
    DT = NULL,
    coords = NULL,
    group = 'group',
    na.rm = FALSE) {

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (is.null(group)) {
    stop('group column name required')
  }

  if (any(!(
    c(coords, group) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(coords, group),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  if (is.null(na.rm)) {
    stop('na.rm is required')
  }

  if (!is.logical(na.rm)) {
    stop('na.rm should be a boolean (TRUE/FALSE), see ?mean')
  }

  xcol <- data.table::first(coords)
  ycol <- data.table::last(coords)

  out_xcol <- paste0('centroid_', gsub(' ', '', xcol))
  out_ycol <- paste0('centroid_', gsub(' ', '', ycol))

  if (out_xcol %in% colnames(DT)) {
    message(paste(out_xcol, 'column will be overwritten by this function'))
    data.table::set(DT, j = out_xcol, value = NULL)
  }

  if (out_ycol %in% colnames(DT)) {
    message(paste(out_ycol, 'column will be overwritten by this function'))
    data.table::set(DT, j = out_ycol, value = NULL)
  }

  DT[, c(out_xcol) := mean(.SD[[xcol]], na.rm = na.rm), by = c(group)]
  DT[, c(out_ycol) := mean(.SD[[ycol]], na.rm = na.rm), by = c(group)]

  return(DT[])
}
