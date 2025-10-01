#' Get geometry
#'
#' `get_geometry` sets up an input `DT` with a 'geometry' column for {spatsoc}'s
#' {sf} interface. The function expects a `data.table` with relocation data and
#' a coordinate reference system.
#'
#' The `DT` must be a `data.table`. If your data is a `data.frame`, you can
#' convert it by reference using [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' The `coords` argument expects the names of columns in `DT` which correspond
#' to the X and Y coordinates.
#'
#' @return `get_geometry` returns the input `DT` appended with a
#'   `geometry` column which represents the input coordinates
#'   as a `sfc` (simple feature geometry list column). If the `output_crs`
#'   was provided, the `geometry` will be transformed to the `output_crs`.
#'
#'   A message is returned when a column named `geometry` already exists in
#'   the input `DT`, because it will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @inheritParams group_pts
#' @inheritParams group_polys
#' @param output_crs default 4326, the output crs to transform the input
#'   coordinates into. If output_crs is NULL or FALSE or matching the crs argument,
#'   the coordinates will not be transformed
#' @param geometry_colname default "geometry", optionally set output colname for
#'   geometry column
#'
#' @export
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file('extdata', 'DT.csv', package = 'spatsoc'))
#'
#' # Get geometry
#' get_geometry(DT, coords = c('X', 'Y'), crs = 32736)
get_geometry <- function(
    DT = NULL,
    coords = NULL,
    crs = NULL,
    output_crs = 4326,
    geometry_colname = 'geometry') {
