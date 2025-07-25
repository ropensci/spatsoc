#' Difference of two angles measured in radians
#'
#' Internal function
#'
#' @param x angle in radians
#' @param y angle in radians
#' @param signed boolean if signed difference should be returned, default FALSE
#' @param return_units return difference with units = 'rad'
#'
#' @return Difference between x and y in radians. If signed is TRUE, the signed
#'   difference is returned. If signed is FALSE, the absolute difference is
#'   returned. Note: The difference is the smallest difference, eg.
#' @references adapted from https://stackoverflow.com/a/7869457
#' @keywords internal
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
#' # Differences
#' spatsoc:::diff_rad(DT[1, direction], DT[2, direction])
 diff_rad <- function(x, y,  signed = FALSE, return_units = FALSE) {
  if (!inherits(x, 'units') || units(x)$numerator != 'rad') {
    stop('units(x) is not radians')
  }
  if (!inherits(y, 'units') || units(y)$numerator != 'rad') {
    stop('units(y) is not radians')
  }

  d <- units::drop_units(y) - units::drop_units(x)
  d <- ((d + pi) %% (2 * pi)) - pi

  if (signed) {
    out <- d
  } else {
    out <- abs(d)
  }

  if (return_units) {
    return(units::as_units(out, 'rad'))
  } else {
    return(out)
  }
}
