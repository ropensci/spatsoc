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
delta_rad <- function(target, source,  signed = FALSE) {
  if (!inherits(target, 'units') || units(target)$numerator != 'rad') {
    stop('units(targets) is not radians')
  }
  if (!inherits(source, 'units') || units(source)$numerator != 'rad') {
    stop('units(source) is not radians')
  }

  d <- source - target
  d <- (d + pi) %% (2 * pi) - pi

  if (signed) {
    return(d)
  } else {
    return(abs(d))
  }
}
