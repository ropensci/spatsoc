#' Difference of two angles measured in radians
#'
#' Internal function
#'
#' @param target angle in radians
#' @param source angle in radians
#' @param signed boolean if signed difference should be returned, default FALSE
#'
#' @return
#' @references adapted from https://stackoverflow.com/a/7869457
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
#'
#' # Differences
#' DT[, diff_rad(direction[1], direction[2])]
diff_rad <- function(x, y,  signed = FALSE) {
  if (!inherits(x, 'units') || units(x)$numerator != 'rad') {
    stop('units(x) is not radians')
  }
  if (!inherits(y, 'units') || units(y)$numerator != 'rad') {
    stop('units(y) is not radians')
  }

  d <- y - x
  pi_rad <- units::as_units(pi, 'rad')
  d <- ((d + pi_rad) %% (2 * pi_rad)) - pi_rad

  if (signed) {
    return(d)
  } else {
    return(abs(d))
  }
}


# Requires version with this PR merged
# remotes::install_github('https://github.com/r-quantities/units/pull/365')
