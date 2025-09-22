# TODO: check classes input in calling function
calc_centroid <- function(geometry, x, y, na.rm = TRUE) {
  if (!missing(geometry) & missing(x) & missing(y)) {
    if (length(geometry) > 1L) {
      sf::st_sf(sf::st_centroid(sf::st_combine(geometry)))
    } else {
      sf::st_sf(sf::st_sfc(sf::st_point(), crs = sf::st_crs(geometry)))
    }
  } else if (missing(geometry) & !missing(x) & !missing(y)){
    if (length(x) > 1L & length(y) > 1L) {
      list(mean(x, na.rm = na.rm), mean(y, na.rm = na.rm))
    }
  }
}
