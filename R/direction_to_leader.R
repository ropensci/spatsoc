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
