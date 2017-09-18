#' Group Times
#'
#' Assign a numerical group for rows by provided time column. If
#' using GPS collar data, it is recommended to round this time column providing
#' the rounding unit as described below.
#'
#' The rounding unit must be provided....
#'
#' Note that this numerical time group ID is added to the provided data.table.
#'
#' @param DT input locs/rows
#' @param timeField time column name
#' @param timeThreshold threshold for grouping times. eg: '2 hours', '10 minutes', etc.
#'                      if not provided, times will be matched exactly. Note that provided
#'                      threshold must be in the expected format: ## unit
#'
#' @export
GroupTimes <- function(DT, timeField, timeThreshold = NULL) {
  if(any(!(c(timeField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(is.null(timeThreshold)) {
    DT[, timeGroup := .GRP, by = timeField]
  } else {
    if(grepl('hour', timeThreshold)){
# if 1 hour, go to 60 minutes
      data.table::as.ITime(data.table::tstrsplit(timeThreshold, ' ', type.convert = TRUE, keep = 1),
                           format = '%H')
    } else if(grepl('minute', timeThreshold)){

      nTime <- unlist(data.table::tstrsplit(timeThreshold, ' ', keep = 1, type.convert = TRUE))

      # alloc.col(DT, 1)

      DT[(data.table::minute(get(timeField)) %% nTime) > (nTime / 2),
         newtime := as.POSIXct(get(timeField)) +
                (nTime - (data.table::minute(get(timeField)) %% nTime)) * 60]
      DT[is.na(newtime), newtime := as.POSIXct(get(timeField))]

      return(DT)

      # by = .(hour(V1), yday(V1), minute(V1))


      # DT[is.null(newtime), newtime := get(timeField)]
      # return(DT[])
    }
  }
}

# TODO: Update description above..

# select only those rows with minute > 30, add 30 to itime
# take all itimes (with new ones) and pull out unit indicated

### sub in function for:
# round to nearest hour? minute? 5 minutes? (how) take minutes and divide and round?
# if modulus 5 is > 2.5 then add

# if no rounding, provide daily groups etc

# DT[data.table::minute(itime) > 30, itime + as.ITime('00:30:00')]
