#' Direction to group centroid
#'
#' `direction_to_centroid` calculates the direction of each relocation to the
#' centroid of the spatiotemporal group identified by `group_pts`. The function
#' expects a `data.table` with relocation data appended with a `group` column
#' from `group_pts` and centroid columns from `centroid_group`. Relocation data
#' should be in two columns representing the X and Y coordinates, or in a
#' geometry column prepared by the helper function [get_geometry()].
#'
#' The `DT` must be a `data.table`. If your data is a `data.frame`, you can
#' convert it by reference using [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' This function expects a `group` column present generated with the `group_pts`
#' function and centroid coordinates generated with the `centroid_group`
#' function. The `group` argument expects the name of the column in `DT` which
#' correspond to the group column.
#'
#' See below under "Interface" for details on providing coordinates and under
#' "Direction function" for details on the underlying direction function used.
#'
#' @inheritSection direction_step Interface
#' @inheritSection direction_step Direction function
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
#'   A message is returned when `direction_centroid` column already exist in the
#'   input `DT`, because they will be overwritten.
#'
#'   Missing values in coordinates / geometry are ignored and NA is returned.
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
#' centroid_group(DT, coords = c('X', 'Y'), group = 'group')
#'
#' # Calculate direction to group centroid
#' direction_to_centroid(DT, coords = c('X', 'Y'), crs = 32736)
direction_to_centroid <- function(
    DT = NULL,
    coords = NULL,
    crs = NULL,
    geometry = 'geometry') {

  # Due to NSE notes in R CMD check
  geo <- cent <- x <- y <- x_centroid <- y_centroid <- NULL

  assert_not_null(DT)
  assert_is_data_table(DT)

  out_colname <- 'direction_centroid'

  if (is.null(coords)) {
    if (!is.null(crs)) {
      message('crs argument is ignored when coords are null, using geometry')
    }

    assert_are_colnames(DT, geometry, ', did you run get_geometry()?')
    assert_col_inherits(DT, geometry, 'sfc_POINT')
    centroid_col <- 'centroid'
    assert_are_colnames(DT, centroid_col, ', did you run centroid_group?')
    assert_col_inherits(DT, centroid_col, 'sfc_POINT')

    if (out_colname %in% colnames(DT)) {
      message(out_colname, ' column will be overwritten by this function')
      data.table::set(DT, j = out_colname, value = NULL)
    }

    crs <- sf::st_crs(DT[[geometry]])
    use_transform <- !sf::st_is_longlat(crs)

    if (is.na(use_transform)) {
      rlang::abort(paste0('sf::st_is_longlat(crs) is ', use_transform, ', ensure crs is provided for direction functions'))
    }

    DT[!sf::st_is_empty(geo) & !sf::st_is_empty(cent),
      c(out_colname) := calc_direction(
        geometry_a = geo,
        geometry_b = cent,
        use_transform = use_transform
      ),
      env = list(geo = geometry, cent = centroid_col)
    ]

  } else {
    assert_are_colnames(DT, coords)
    assert_length(coords, 2)
    assert_col_inherits(DT, coords, 'numeric')
    assert_not_null(crs)

    xcol <- data.table::first(coords)
    ycol <- data.table::last(coords)
    pre <- 'centroid_'
    xcol_centroid <- paste0(pre, xcol)
    ycol_centroid <- paste0(pre, ycol)
    coords_centroid  <- c(xcol_centroid, ycol_centroid)

    assert_are_colnames(DT, coords_centroid, ', did you run centroid_group?')
    assert_col_inherits(DT, coords_centroid, 'numeric')

    if (out_colname %in% colnames(DT)) {
      message(out_colname, ' column will be overwritten by this function')
      data.table::set(DT, j = out_colname, value = NULL)
    }

    use_transform <- !sf::st_is_longlat(crs)

    if (is.na(use_transform)) {
      rlang::abort(paste0('sf::st_is_longlat(crs) is ', use_transform,
                          ', ensure crs is provided for direction functions'))
    }

    DT[!is.na(x) & !is.na(y) & !is.na(x_centroid) & !is.na(y_centroid),
      c(out_colname) := calc_direction(
        x_a = x, y_a = y,
        x_b = x_centroid, y_b = y_centroid,
        crs = crs,
        use_transform = use_transform
      ),
      env = list(
        x = xcol, y = ycol,
        x_centroid = xcol_centroid, y_centroid = ycol_centroid
      )
    ]
  }

  return(DT[])
}
