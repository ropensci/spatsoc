# https://stackoverflow.com/a/7869457
delta_rad <- function(target, source,  signed = FALSE) {
  d <- source - target
  d <- (d + pi) %% (2 * pi) - pi
  if (signed) {
    return(d)
  } else {
    return(abs(d))
  }
}

