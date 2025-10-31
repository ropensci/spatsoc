#' Direction to group leader
#'
#' `direction_to_leader` calculates the direction to the leader of each
#' spatiotemporal group. The function expects a `data.table` with
#' relocation data appended with a `rank_position_group_direction` column
#' indicating the ranked position along the group direction generated with
#' `leader_direction_group(return_rank = TRUE)`. Relocation data should be
#' in planar coordinates provided in two columns representing the X and Y
#' coordinates.
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()] or by reassigning using
#' [data.table::data.table()].
#'
#' This function expects a `rank_position_group_direction` column
#' generated with `leader_direction_group(return_rank = TRUE)`,
#' a `group` column generated with the
#' `group_pts` function. The `coords` and `group` arguments
#' expect the names of columns in `DT` which correspond to the X and Y
#' coordinates and group columns.
#'
#' @inheritParams distance_to_leader
#' @inheritParams direction_step
#'
#' @return `direction_to_leader` returns the input `DT` appended with
#'   a `direction_leader` column indicating the direction to the group
#'   leader. A value of NaN is returned when the coordinates of the focal
#'   individual equal the coordinates of the leader.
#'
#'   A message is returned when the `direction_leader` column already
#'   exist in the input `DT` because it will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#' @export
#' @family Direction functions
#' @family Leadership functions
#' @seealso [distance_to_leader], [leader_direction_group], [group_pts]
#' @references
#'
#' See examples of using direction to leader and position within group:
#'  * \doi{doi:10.1016/j.anbehav.2023.09.009}
#'  * \doi{doi:10.1016/j.beproc.2013.10.007}
#'  * \doi{doi:10.1371/journal.pone.0036567}
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # (Subset example data to reduce example run time)
#' DT <- DT[year(datetime) == 2016]
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '20 minutes')
#'
#' # Spatial grouping with timegroup
#' group_pts(DT, threshold = 50, id = 'ID',
#'           coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Calculate direction at each step
#' direction_step(
#'   DT = DT,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   crs = 32736
#' )
#'
#' # Calculate group centroid
#' centroid_group(DT, coords = c('X', 'Y'))
#'
#' # Calculate group direction
#' direction_group(DT)
#'
#' # Calculate leader in terms of position along group direction
#' leader_direction_group(
#'   DT,
#'   coords = c('X', 'Y'),
#'   return_rank = TRUE
#' )
#'
#' # Calculate direction to leader
#' direction_to_leader(DT, coords = c('X', 'Y'), crs = 32736)
direction_to_leader <- function(
    DT = NULL,
    coords = NULL,
    group = 'group',
    crs = NULL) {
  # Due to NSE notes
  direction_leader <- rank_position_group_direction <- has_leader <-
    zzz_N_by_group <- . <- NULL

  assert_not_null(DT)
  assert_is_data_table(DT)
  assert_not_null(group)
  assert_are_colnames(DT, group)
  assert_not_null(crs)

  assert_not_null(coords)
  assert_are_colnames(DT, coords)
  assert_length(coords, 2)
  assert_col_inherits(DT, coords, 'numeric')

  leader_col <- 'rank_position_group_direction'
  assert_are_colnames(DT, leader_col,
                      ', did you run leader_direction_group(return_rank = TRUE)?')
  assert_col_inherits(DT, leader_col, 'numeric')

  out_col <- 'direction_leader'
  if (out_col %in% colnames(DT)) {
    message(
      paste0(out_col, ' column will be overwritten by this function')
    )
    data.table::set(DT, j = out_col, value = NULL)
  }

  check_leaderless <- DT[, .(
    has_leader = any(rank_position_group_direction == 1)),
    by = c(group)][!(has_leader)]
  xcol <- data.table::first(coords)
  ycol <- data.table::last(coords)
  pre <- 'zzz_leader_'
  zzz_leader_x <- paste0(pre, xcol)
  zzz_leader_y <- paste0(pre, ycol)
  zzz_leader_coords  <- c(zzz_leader_x, zzz_leader_y)

  if (check_leaderless[, .N > 0]) {
    warning(
      'groups found missing leader (rank_position_group_direction == 1): \n',
      check_leaderless[, paste(group, collapse = ', ')]
    )
  }

  DT[, c(zzz_leader_coords) :=
       .SD[which(rank_position_group_direction == 1)],
     .SDcols = c(coords),
     by = c(group)]

  DT[!group %in% check_leaderless$group,, direction_leader := calc_direction(
    x_a = .SD[[xcol]],
    y_a = .SD[[ycol]],
    x_b = .SD[[zzz_leader_x]],
    y_b = .SD[[zzz_leader_y]],
    crs = crs
    )]

  data.table::set(DT, j = zzz_leader_coords, value = NULL)

  return(DT[])
}
