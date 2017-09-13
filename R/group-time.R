#' Group Time
#'
#' Assign a numerical group for rows by provided time column. If
#' using GPS collar data, it is recommended to round this time column providing
#' the rounding unit as described below.
#'
#' The rounding unit must be provided....
#'
#' Note that this numerical time group ID is added to the provided data.table.
#'
#' @param dt input locs/rows
#' @param timeField time column name
#' @param roundBool (optional) unit of time to round on (eg: day, hour, minute)
#'
#' @export
GroupTime <- function(dt, timeField, roundField = NULL) {
  if(any(!(c(timeField, roundField) %in% colnames(dt)))){
    stop('some fields provided are not present in data.table provided/colnames(dt)')
  }
  if(!(roundField)) {
    dt[, timeGroup := .GRP, by = timeField]
  } else {
    if(roundField == 'hour'){

      # select only those rows with minute > 30, add 30 to itime
      # take all itimes (with new ones) and pull out unit indicated

      ### sub in function for:
      # round to nearest hour? minute? 5 minutes? (how) take minutes and divide and round?
      # if modulus 5 is > 2.5 then add

      # if no rounding, provide daily groups etc

      dt[data.table::minute(itime) > 30, itime + as.ITime('00:30:00')]
    }


  }
}

# TODO: Update description above..
