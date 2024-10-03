#' Group centroid
#'
#' Mean of individual locations in each group
#'
#' @param DT expecting group generated with eg. group_pts
#' @param coords character vector of column names for x, y
#' @param group group column default 'group'
#' @param na.rm if NAs should be removed in calculating mean location
centroid_group <- function(DT, coords, group = 'group', na.rm = FALSE) {
  stopifnot(length(coords) == 2)

  xcol <- first(coords)
  ycol <- last(coords)

  stopifnot(xcol %in% colnames(DT))
  stopifnot(ycol %in% colnames(DT))
  stopifnot(group %in% colnames(DT))

  DT[, paste0('group_mean_', xcol) := mean(.SD[[xcol]], na.rm = na.rm), by = c(group)]
  DT[, paste0('group_mean_', ycol) := mean(.SD[[ycol]], na.rm = na.rm), by = c(group)]
  return(DT[])
}
