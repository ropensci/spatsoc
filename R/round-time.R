#' Round Time
#'
#' @param dt input locs/rows
#' @param timeField time column name
#' @param roundUnit unit of time to round on (eg: day, hour, minute)
#'
#' @export
RoundTime <- function(dt, timeField, roundUnit = '1 hour') {
  if(any(!(c(timeField, roundField) %in% colnames(dt)))){
    stop('some fields provided are not present in data.table provided/colnames(dt)')
  }

  if(grepl('hour', roundUnit)){
    nTime <- data.table::as.ITime(strsplit(roundUnit, 'hour')[1], format = '%H')
  } else if(grepl('minute', roundUnit)){
    nTime <- data.table::as.ITime(strsplit(roundUnit, 'minute')[1], format = '%M')
  } else {
    stop('must round to nearest hour or minute (as determined by roundUnit variable)')
  }


  roundUnit = data.table::as.ITime('01:00:00')
  locs[(data.table::minute(itime) %% data.table::minute(roundUnit)) >
         (data.table::minute(roundUnit) / 2),
       .(ID, idate, itime,
         itime + roundUnit)]
  minute(roundUnit)

  locs[minute(itime) > 30, .(itime, hour = hour(itime))]
  # timePlus30 = itime + as.ITime('00:30:00'),
  # hourPlus30 = hour(itime + as.ITime('00:30:00')))]

  # if there are no rows, then it returns the origin must be supplied error..

}

# TODO: double check the >= <= < >
