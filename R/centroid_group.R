#' Group centroid
#'
#' `centroid_group` calculates the centroid of all
#' individuals in each spatiotemporal group identified by `group_pts`. The
#' function expects a `data.table` with relocation data appended with a
#' `group` column from `group_pts`. Relocation data
#' should be in two columns representing the X and Y coordinates, or in a
#' geometry column prepared by the helper function [get_geometry()].
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' The `group` argument expects the name of a column in
#' `DT` which correspond to the group column.
#'
#' See below under Interface for details on providing coordinates.
#'
#' @section Interface:
#'  Two interfaces are available for providing coordinates:
#'
#'  1. Provide `coords` and optionally `crs`. The `coords` argument expects the
#'  names of the X and Y coordinate columns. The `crs` argument expects a
#'  character string or numeric defining the coordinate reference system to be
#'  passed to [sf::st_crs]. For example, for UTM zone 36S (EPSG 32736), the crs
#'  argument is `crs = "EPSG:32736"` or `crs = 32736`. See
#'  <https://spatialreference.org> for a list of EPSG codes. For centroid
#'  calculations, if `crs` is NULL, it will be internally set to `NA_crs_`. 2.
#'  (New!) Provide `geometry`. The `geometry` argument allows the user to supply
#'  a `geometry` column that represents the coordinates as a simple feature
#'  geometry list column. This interface expects the user to prepare their input
#'  DT with [get_geometry()]. To use this interface, leave the `coords` and
#'  `crs` arguments `NULL`, and the default argument for `geometry` ('geometry')
#'  will be used directly.
#'
#'
#' @param DT input data.table with group column generated with `group_pts`
#' @inheritParams group_pts
#' @param group Character string of group column
#' @inheritParams direction_step
#'
#' @return `centroid_group` returns the input `DT` appended with
#'  centroid column(s) for each group.
#'
#'  If the `crs` for `coords` or `st_crs(geometry)` for `geometry` is long lat
#'  (see [sf::st_is_longlat()]), centroids will be calculated using
#'  [s2::s2_centroid()] through [sf::st_centroid()]. If the `crs` for `coords`
#'  or `st_crs(geometry)` for `geometry` is projected or NA, the centroids will
#'  be calculated using a mean on the coordinates.
#'
#'  If `coords` are provided, the centroid columns will be named by prefixing
#'  the coordinate column names with "centroid_" (eg. "X" = "centroid_X"). If
#'  `geometry` is used, the centroid column will be named "centroid".
#'
#'  Note: due to the merge required within this function, the output needs to be
#'  reassigned unlike some other `spatsoc` functions like `dyad_id`
#'  and `group_pts`. See details in
#'  [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#'   A message is returned when the centroid column(s) already exist in the input
#'   because they will be overwritten.
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
