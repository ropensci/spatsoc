#' Polarization
#'
#' \code{direction_polarization} calculates the polarization of individual
#' directions in each spatiotemporal group identified by \code{group_pts}. The
#' function expects a \code{data.table} with relocation data appended with a
#' \code{direction} column from \code{direction_step} and a \code{group} column
#' from \code{group_pts}.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' The \code{direction} and \code{group} arguments expect the names of columns
#' in \code{DT} which correspond to the direction and group columns. The
#' direction column is expected in units of radians and the polarization is
#' calculated with [CircStats::r.test()].
#'
#' @inheritParams direction_group
#'
#' @return \code{direction_polarization} returns the input \code{DT} appended
#'   with a \code{polarization} column representing the direction polarization
#'   of all individuals in each spatiotemporal group.
#'
#'   The direction polarization is calculated using [CircStats::r.test()]
#'   which expects units of radians.
#'
#'   A message is returned when the \code{polarization} columns already
#'   exists in the input \code{DT}, because it will be overwritten.
#'
#' @export
#' @seealso \code{\link{direction_step}}, \code{\link{group_pts}},
#'   [CircStats::r.test()]
#' @family Direction functions
#'
#' @references
#' See examples of using polarization:
#'  * <https://doi.org/10.1016/j.cub.2017.08.004>
#'  * <https://doi.org/10.1371/journal.pcbi.1009437>
#'  * <https://doi.org/10.7554/eLife.19505>
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
#' group_pts(DT, threshold = 50, id = 'ID',
#'           coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Calculate direction at each step
#' direction_step(
#'   DT = DT,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   projection = 32736
#' )
#'
#' # Calculate polarization
#' direction_polarization(DT)
direction_polarization <- function(
    DT,
    direction = 'direction',
    group = 'group') {

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(direction)) {
    stop('direction column name required')
  }

  if (is.null(group)) {
    stop('group column name required')
  }

  if (any(!(
    c(direction, group) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(direction, group),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = c(direction)]))) {
    stop('direction must be numeric')
  }

  out <- 'polarization'

  if (out %in% colnames(DT)) {
    message(paste(out, 'column will be overwritten by this function'))
    data.table::set(DT, j = out, value = NULL)
  }

  if (DT[, !inherits(.SD[[1]], 'units'), .SDcols = c(direction)] ||
      DT[, units(.SD[[1]])$numerator != 'rad', .SDcols = c(direction)]) {
    stop('units(DT$direction) is not radians, did you use direction_step?')
  }

  DT[, c(out) :=
       CircStats::r.test(units::drop_units(.SD[[1]]), degree = FALSE)$r.bar,
    by = c(group),
    .SDcols = c(direction)]

  return(DT[])
}


