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
  if (!inherits(target, 'units') || units(target)$numerator != 'rad') {
    stop('units(targets) is not radians')
diff_rad <- function(x, y,  signed = FALSE) {
  }
  if (!inherits(source, 'units') || units(source)$numerator != 'rad') {
    stop('units(source) is not radians')
  }

  d <- source - target
  pi_rad <- units::as_units(pi, 'rad')
  d <- (d + pi_rad) %% (2 * pi_rad) - pi_rad

  if (signed) {
    return(d)
  } else {
    return(abs(d))
  }
}


# Requires version with this PR merged
# remotes::install_github('https://github.com/r-quantities/units/pull/365')
