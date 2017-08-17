#' Time Groups
#'
#' Assign a numerical group ID for rows/locs sorted by provided time column. If
#' using GPS collar data, it is recommended to round this time column providing
#' the rounding unit as described below.
#'
#' The rounding unit must be provided
#'
#' Note that this numerical time group ID is added to the provided data.table.
#'
#' @param dt input locs/rows
#' @param time time column name
#' @param roundunit unit by which the time should be rounded (optional)
#'
#' @export
TimeGroups <- function(dt, time, roundunit = NULL) {
  if(is.null(roundunit)) {
    dt[, timeGroup := .GRP, by = time]
  } else {
    dt[, timeGroup := .GRP, by = round(time)]
  }
}
