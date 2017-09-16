#' Round Time
#'
#' @param DT input locs/rows
#' @param timeField time column name
#' @param roundUnit unit of time to round on (eg: day, hour, minute)
#'
#' @export
RoundTime <- function(DT, timeField, roundUnit = '1 hour', dateField) {
  if(any(!(c(timeField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }

  # print(typeof(DT[, get(timeField)]))

  if(grepl('hour', roundUnit)){
    # nTime <- data.table::as.ITime(strsplit(roundUnit, ' hour')[[1]], format = '%H')
    # nTime <- as.integer(strsplit(roundUnit, ' hour')[[1]]) * 60

    # nTime <-
    DT[, as.POSIXct(get(dateField)) +
         data.table::as.ITime(data.table::hour(get(timeField) + 30), origin = '1960-01-01')]

    # what about time zones

  #####
    # nTime <- as.integer(strsplit(roundUnit, ' hour')[[1]]) * 60
    # DT[, .(data.table::hour(get(timeField)) + (data.table::as.ITime(nTime / 2, '%M')))]
  ####



    # if(nrow(
    # toRound <- DT[, .(.I, itime)][data.table::minute(itime) > (nTime / 2), I]
    #
    # hours <- DT[, .(h = data.table::hour(get(timeField)))]
    # hours[toRound, h := h + data.table(as.ITime('00:30:00'))]
    # data.table::set(DT, toRound,
    #                 'roundtime', data.table::hour(DT[[timeField]]) + data.table::as.ITime('00:30:00'))
    # DT[is.na(roundtime), roundtime := data.table::hour(get(timeField))]
      # ) == 0){

    #   DT[, .(roundtime = data.table::hour(get(timeField)))]
    # } else {
    #   DT[, .(roundtime = ifelse(data.table::minute(get(timeField)) > (nTime / 2),
    #                             data.table::hour(get(timeField)) + data.table::as.ITime('00:30:00'),
    #                             data.table::hour(get(timeField)) ))]

  } else if(grepl('minute', roundUnit)){
    nTime <- data.table::as.ITime(strsplit(roundUnit, ' minute')[[1]], format = '%M')
  } else {
    stop('must round to nearest hour or minute (as determined by roundUnit variable)')
  }

  # nTime
  # roundUnit = data.table::as.ITime('01:00:00')
  # locs[(data.table::minute(itime) %% data.table::minute(roundUnit)) >
  #        (data.table::minute(roundUnit) / 2),
  #      .(ID, idate, itime,
  #        itime + roundUnit)]
  # minute(roundUnit)
  #
  # locs[minute(itime) > 30, .(itime, hour = hour(itime))]


  # timePlus30 = itime + as.ITime('00:30:00'),
  # hourPlus30 = hour(itime + as.ITime('00:30:00')))]

  # if there are no rows, then it returns the origin must be supplied error..

}

# TODO: double check the >= <= < >
