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
#'
#' The \code{edges} and \code{DT} are internally merged in this function using
#' the columns \code{timegroup} (from \code{group_times}) and \code{ID1} and
#' \code{ID2} (in \code{edges}, from \code{dyad_id}) and \code{id} (in
#' \code{DT}). This function expects a \code{fusionID} present, generated with
#' the \code{fusion_id} function. The \code{timegroup} argument expects the
#' names of a column in \code{edges} which correspond to the timegroup column.
#' The \code{id}, \code{coords} and \code{timegroup} arguments expect the names
#' of a column in \code{DT} which correspond to the id, X and Y coordinates and
#' timegroup columns. The \code{na.rm} argument is passed to the \code{rowMeans}
#' function to control if NA values are removed before calculation.
#'
