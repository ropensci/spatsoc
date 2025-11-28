#' Group centroid
#'
#' `centroid_group` calculates the centroid of all
#' individuals in each spatiotemporal group identified by `group_pts`. The
#' function expects a `data.table` with relocation data appended with a
#' `group` column from `group_pts`. Relocation data should be in two
#' columns representing the X and Y coordinates.
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' The `coords` and `group` arguments expect the names of a column in
#' `DT` which correspond to the X and Y coordinates and group columns.
#'
#' @param DT input data.table with group column generated with `group_pts`
#' @inheritParams group_pts
#' @param group Character string of group column
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
#' @seealso `group_pts`
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
#' centroid_group(DT, coords = c('X', 'Y'), group = 'group')
centroid_group <- function(
    DT = NULL,
    coords = NULL,
    crs = NULL,
    group = 'group',
    geometry = 'geometry') {

  # Due to NSE notes in R CMD check
  x <- y <- NULL

  assert_not_null(DT)
  assert_is_data_table(DT)

  assert_not_null(group)
  assert_are_colnames(DT, group)

  if (is.null(coords)) {
    if (!is.null(crs)) {
      message('crs argument is ignored when coords are null, using geometry')
    }

    assert_are_colnames(DT, geometry, ', did you run get_geometry()?')
    assert_col_inherits(DT, geometry, 'sfc_POINT')

    out <- 'centroid'
    if (out %in% colnames(DT)) {
      message(out, ' column will be overwritten by this function')
      data.table::set(DT, j = out, value = NULL)
    }

    use_mean <- crs_use_mean(sf::st_crs(DT[[geometry]]))

    DT[, (out) := calc_centroid(geo, use_mean = use_mean),
       env = list(geo = geometry),
       by = c(group)]
    DT[, (out) := sf::st_sfc(cent, recompute_bbox = TRUE),
       env = list(cent = out)]

  } else {
    if (is.null(crs)) {
      crs <- sf::NA_crs_
    }

    assert_are_colnames(DT, coords)
    assert_length(coords, 2)
    assert_col_inherits(DT, coords, 'numeric')

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
    use_mean <- crs_use_mean(crs)
    DT[, (c(out_xcol, out_ycol)) :=
         calc_centroid(, x, y, crs = crs, use_mean = use_mean),
       env = list(x = xcol, y = ycol),
       by = c(group)]
  }

  return(DT[])
}
