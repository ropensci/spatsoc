#' Calculate absolute bearing to group centroid
#'
#' @param DT expects group_mean columns generated with group_centroid
#' @param coords character vector of column names for x, y
#' @export
bearing_to_group_centroid <- function(DT, coords = NULL) {
  pre <- 'group_mean_'

  stopifnot(length(coords) == 2)

  xcol <- first(coords)
  ycol <- last(coords)
  group_xcol <- paste0(pre, xcol)
  group_ycol <- paste0(pre, ycol)

  stopifnot(xcol %in% colnames(DT))
  stopifnot(ycol %in% colnames(DT))
  stopifnot(group_xcol %in% colnames(DT))
  stopifnot(group_ycol %in% colnames(DT))

  DT[, bearing_centroid := fifelse(
    .SD[[xcol]] == .SD[[group_xcol]] &
      .SD[[ycol]] == .SD[[group_ycol]],
    NaN,
    atan2(.SD[[group_ycol]] - .SD[[ycol]],
          (.SD[[group_xcol]] - .SD[[xcol]]))
  )]
  return(DT[])
}
