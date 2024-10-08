#' Group mean direction
#'
#' \code{direction_group} calculates the mean direction of all individuals in
#' each spatiotemporal group identified by \code{group_pts}. The function
#' accepts a \code{data.table} with relocation data appended with a
#' \code{direction} column from \code{direction_step} and a \code{group} column
#' from \code{group_pts}.
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' The \code{direction} and \code{group} arguments expect the names of columns
#' in \code{DT} which correspond to the direction and group columns. The
#' \code{na.rm} argument is passed to the \code{mean} function to control if NA
#' values are removed before calculation.
