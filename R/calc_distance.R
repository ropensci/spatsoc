calc_distance <- function(
  geometry_a, geometry_b,
  x_a, y_a,
  x_b, y_b) {
  if (!missing(geometry_a) & missing(x_a) & missing(y_a)) {
    if (!missing(geometry_b)) {
      sf::st_distance(geometry_a, geometry_b, by_element = TRUE)
    } else {
      sf::st_distance(geometry_a, by_element = FALSE)
    }
  } else if (missing(geometry_a) & missing(geometry_b) &
             !missing(x_a) & !missing(y_a)) {
    if (!missing(x_b) & !missing(y_b)) {
      sqrt((x_a - x_b) ^ 2 + (y_a - y_b) ^ 2)
    } else {
      stats::dist(cbind(x_a, y_a), method = 'euclidean')
    }
  }
}
