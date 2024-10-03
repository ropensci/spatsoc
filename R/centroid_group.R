#' Group centroid
#'
#' \code{centroid_group} calculates the centroid (mean location) of
#' all individuals in each spatiotemporal group identified by
#' \code{group_pts}. The function accepts a \code{data.table} with
#' relocation data appended with a
#'   \code{group} column from \code{group_pts}. Relocation data should be
#' in two columns representing the X and Y coordinates.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}}.
#'
#' The \code{coords} and \code{group}
#' arguments expect the names of a column in \code{DT} which correspond to the
#' X and Y coordinates and group columns. The \code{na.rm} argument is passed
#' to the \code{mean} function to control if NA values are removed before
#' calculation.
#'
#' @param DT input data.table with group column generated with \code{group_pts}
#' @inheritParams group_pts
#' @param group Character string of group column
#' @param na.rm if NAs should be removed in calculating mean location, see \code{mean}
#'
#' @return \code{centroid_group} returns the input \code{DT} appended with
#'  centroid columns for the X and Y coordinate columns.
#'
#'   These columns represents the centroid coordinate columns.
#'   The naming of these columns will correspond to the provided coordinate
#'   column names prefixed with "centroid_".
#'
#'   A message is returned when centroid columns are already exists in
#'   the input \code{DT}, because they will be overwritten.

  xcol <- first(coords)
  ycol <- last(coords)

  stopifnot(xcol %in% colnames(DT))
  stopifnot(ycol %in% colnames(DT))
  stopifnot(group %in% colnames(DT))

  DT[, paste0('group_mean_', xcol) := mean(.SD[[xcol]], na.rm = na.rm), by = c(group)]
  DT[, paste0('group_mean_', ycol) := mean(.SD[[ycol]], na.rm = na.rm), by = c(group)]
  return(DT[])
}
