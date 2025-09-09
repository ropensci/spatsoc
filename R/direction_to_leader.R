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
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' This function expects a `rank_position_group_direction` column
#' generated with `leader_direction_group(return_rank = TRUE)`,
#' a `group` column generated with the
#' `group_pts` function. The `coords` and `group` arguments
#' expect the names of columns in `DT` which correspond to the X and Y
#' coordinates and group columns.
#'
#' @inheritParams distance_to_leader
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
#'   projection = 32736
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
#' direction_to_leader(DT, coords = c('X', 'Y'))
direction_to_leader <- function(
    DT = NULL,
    coords = NULL,
    group = 'group') {
  # Due to NSE notes
  direction_leader <- rank_position_group_direction <- has_leader <-
    zzz_N_by_group <- . <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(group)) {
    stop('group column name required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (!group %in% colnames(DT)) {
    stop('group column not present in input DT, did you run group_pts?')
  }

  check_cols <- c(coords, group)

  if (any(!(check_cols %in% colnames(DT)))) {
    stop(paste0(
      as.character(paste(setdiff(
        check_cols,
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  leader_col <- 'rank_position_group_direction'

  if (!leader_col %in% colnames(DT)) {
    stop(paste0(
      leader_col,
      ' column not present in input DT, ',
      'did you run leader_direction_group(return_rank = TRUE)?'))
  }

  if (!is.numeric(DT[[leader_col]])) {
    stop(paste0(leader_col, ' column must be numeric'))
  }

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

  if (check_leaderless[, .N > 0]) {
    warning(
      'groups found missing leader (rank_position_group_direction == 1): \n',
      check_leaderless[, paste(group, collapse = ', ')]
    )
  }

  zzz_leader_coords <- c('zzz_leader_xcol', 'zzz_leader_ycol')
  DT[, c(zzz_leader_coords) :=
       .SD[which(rank_position_group_direction == 1)],
     .SDcols = c(coords),
     by = c(group)]

  DT[!group %in% check_leaderless$group,
     direction_leader := fifelse(
    .SD[[1]] == .SD[[3]] &
      .SD[[2]] == .SD[[4]],
    NaN,
    atan2(.SD[[4]] - .SD[[2]], (.SD[[3]] - .SD[[1]]))
  ),
  .SDcols = c(coords, zzz_leader_coords)]

  data.table::set(DT, j = zzz_leader_coords, value = NULL)

  return(DT[])
}
