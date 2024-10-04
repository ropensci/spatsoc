#' Direction to group centroid
#'
#' \code{direction_to_centroid} calculates the direction of each relocation to
#' the centroid of the spatiotemporal group identified by \code{group_pts}. The
#' function accepts a \code{data.table} with relocation data appended with a
#' \code{group} column from \code{group_pts} and centroid columns from
#' \code{centroid_group}. Relocation data should be in two columns representing
#' the X and Y coordinates.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' This function expects a \code{group} column present generated with the
#' \code{group_pts} function and centroid coordinate columns generated with the
#' \code{centroid_group} function. The \code{coords} and \code{group} arguments
#' expect the names of columns in \code{DT} which correspond to the X and Y
#' coordinates and group columns.
#'
#' @inheritParams group_pts

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
