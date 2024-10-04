#' Fusion centroid
#'
#' \code{centroid_fusion} calculates the centroid (mean location) of fusion
#' events. The function accepts an edge list of fusion events identified by
#' \code{fusion_id} from edge lists generated with \code{edge_dist} and a
#' \code{data.table} with relocation data appended with a \code{timegroup}
#' column from \code{group_times}. It is recommended to use the argument
#' \code{fillNA = FALSE} for \code{edge_dist} when using \code{centroid_fusion}
#' to avoid unnecessarily merging additional rows. Relocation data should be in
#' two columns representing the X and Y coordinates.
#'
#' The \code{edges} and \code{DT} must be \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
