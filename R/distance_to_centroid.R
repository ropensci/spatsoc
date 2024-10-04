#' Distance to group centroid
#'
#' @param DT expects group_mean columns generated with group_centroid
#' @param coords character vector of column names for x, y
distance_to_centroid <- function(DT, coords, group = 'group',
                                     return_rank = FALSE) {
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
  stopifnot(group %in% colnames(DT))


  DT[, dist_group_centroid :=
       sqrt((.SD[[xcol]] - .SD[[group_xcol]])^2 +
              (.SD[[ycol]] - .SD[[group_ycol]])^2)]

  if (return_rank) {
    DT[, N_by_group := .N, by = c(group)]
    DT[, rank_dist_group_centroid :=
         data.table::frank(dist_group_centroid),
       by = c(group)]
  }
  return(DT[])
}
