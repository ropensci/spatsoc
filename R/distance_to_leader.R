#' Distance to group leader
#'
#' \code{distance_to_leader} calculates the distance to the leader of each
#' spatiotemporal group. The function accepts a \code{data.table} with
#' relocation data appended with a \code{rank_position_group_direction} column
#' indicating the ranked position along the group direction generated with
#' \code{leader_direction_group(return_rank = TRUE)}. Relocation data should be
#' in planar coordinates provided in two columns representing the X and Y
#' coordinates.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' This function expects a \code{rank_dist_along_group_bearing} column
#' generated with \code{leader_direction_group(return_rank = TRUE)},
#' a \code{group} column generated with the
#' \code{group_pts} function. The \code{coords} and \code{group} arguments
#' expect the names of columns in \code{DT} which correspond to the X and Y
#' coordinates and group columns.
#'
#' @inheritParams leader_direction_group
distance_to_leader <- function(DT, coords = c('x', 'y'), group = 'group') {
  stopifnot(first(coords) %in% colnames(DT))
  stopifnot(last(coords) %in% colnames(DT))
  stopifnot(group %in% colnames(DT))
  stopifnot('rank_dist_along_group_bearing' %in% colnames(DT))

  DT[, temp_N_by_group := .N, by = c(group)]

  check_has_leader <- DT[, .(has_leader = any(rank_dist_along_group_bearing == 1)),
                         by = c(group)][!(has_leader)]

  if (check_has_leader[, .N > 0]) {
    warning('groups found missing leader (rank_dist_along_group_bearing == 1): \n',
            check_has_leader[, paste(group, collapse = ', ')])
  }

  DT[!group %in% check_has_leader$group,
     dist_leader := fifelse(
       temp_N_by_group > 1,
       as.matrix(dist(cbind(.SD[[1]], .SD[[2]])))[, which(.SD[[3]] == 1)],
       0
     ),
     .SDcols = c(coords, 'rank_dist_along_group_bearing'),
     by = c(group)]
  DT[, temp_N_by_group := NULL]
  return(DT)
}
