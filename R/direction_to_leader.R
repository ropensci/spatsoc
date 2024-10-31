#' Direction to group leader
#'
#' \code{direction_to_leader} calculates the direction to the leader of each
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
#' @return \code{direction_to_leader} returns the input \code{DT} appended with
#'   a \code{direction_leader} column indicating the direction to the group leader.
#'
#'   A message is returned when the \code{direction_leader} column is already exist in the input \code{DT}
#'   because it will be overwritten.
#'
#' @export
#' @family Direction functions
#' @seealso [distance_to_leader], [leader_direction_group], [group_pts]
#' @references
#'
#' See examples of using direction to leader and position within group:
#'  * <https://doi.org/10.1016/j.anbehav.2023.09.009>
#'  * <https://doi.org/10.1016/j.beproc.2013.10.007>
#'  * <https://doi.org/10.1371/journal.pone.0036567>
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
#' # Calculate direction to leader
#' direction_to_leader(DT, coords = c('X', 'Y'), group = 'group')
