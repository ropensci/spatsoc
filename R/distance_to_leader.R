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
#' This function expects a \code{rank_position_group_direction} column
#' generated with \code{leader_direction_group(return_rank = TRUE)},
#' a \code{group} column generated with the
#' \code{group_pts} function. The \code{coords} and \code{group} arguments
#' expect the names of columns in \code{DT} which correspond to the X and Y
#' coordinates and group columns.
#'
#' @inheritParams leader_direction_group
#'
#' @return \code{distance_to_leader} returns the input \code{DT} appended with
#'   a \code{distance_leader} column indicating the distance to the group leader.
#'
#'   A message is returned when the \code{distance_leader} column is already exist in the input \code{DT}
#'   because it will be overwritten.
#'
#' @export
#' @family Distance functions
#' @seealso [leader_direction_group], [group_pts]
#' @references
#'
#' See examples of using distance to leader and position within group:
#'  * <https://doi.org/10.1111/jfb.15315>
#'  * <https://doi.org/10.1098/rspb.2017.2629>
#'  * <https://doi.org/10.1016/j.anbehav.2023.09.009>
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
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
#' leader_direction_group(DT, coords = c('X', 'Y'))
#'
#' # Calculate distance to leader
#' distance_to_leader(DT, coords = c('X', 'Y'), group = 'group')
distance_to_leader <- function(
    DT = NULL,
    coords = NULL,
    group = NULL) {
  # Due to NSE notes
  # distance_leader <- zzz_N_by_group <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (is.null(group)) {
    stop('group column name required')
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

  # check exists, is numeric
  leader_col <- 'rank_position_group_direction'

  if (!leader_col %in% colnames(DT)) {
    stop(paste0(
      leader_col,
      ' column not present in input DT, ',
      'did you run leader_direction_group(return_rank = TRUE)?'))
  }

  out_col <- 'distance_leader'
  if (out_col %in% colnames(DT)) {
    message(
      paste0(out_col, ' column will be overwritten by this function')
    )
    data.table::set(DT, j = out_col, value = NULL)
  }


  DT[, zzz_N_by_group := .N, by = c(group)]

  check_has_leader <- DT[, .(
    has_leader = any(rank_position_group_direction == 1)),
    by = c(group)][!(has_leader)]

  if (check_has_leader[, .N > 0]) {
    warning(
      'groups found missing leader (rank_position_group_direction == 1): \n',
      check_has_leader[, paste(group, collapse = ', ')]
    )
  }

  DT[!group %in% check_has_leader$group,
     c(out_col) := fifelse(
       zzz_N_by_group > 1,
       as.matrix(
         stats::dist(cbind(.SD[[1]], .SD[[2]]))
       )[, which(.SD[[3]] == 1)],
       0
     ),
     .SDcols = c(coords, 'rank_position_group_direction'),
     by = c(group)]

  data.table::set(DT, j = 'zzz_N_by_group', value = NULL)

  return(DT[])
}
