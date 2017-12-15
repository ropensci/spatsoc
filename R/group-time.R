#' Group Times
#'
#' Assign a numerical group for rows by provided time column. If
#' using GPS collar data or other data with variability in concurrent measures
#' across individuals, it is recommended to round this time column
#' providing the timeThreshold.
#' Otherwise, rows are grouped by matching exact times.
#'
#' This function can also group rows on time intervals, such as blocks of 5 days.
#' Simply provide the interval in the same manner eg: '5 days'.
#'
#' @param DT input locs/rows
#' @param timeField time column name
#' @param timeThreshold character defining the threshold for grouping times.
#'                      eg: '2 hours', '10 minutes', etc.
#'                      if not provided, times will be matched exactly.
#'                      Note that provided threshold must be in the expected format: '## unit'
#'
#' @export
GroupTimes <- function(DT, timeField, timeThreshold = NULL) {
  if (!truelength(DT)){
    alloc.col(DT)
  }
  if(any(!(c(timeField) %in% colnames(DT)))){
    stop('some fields provided are not present
         in data.table provided/colnames(DT)')
  }

  if('newtime' %in% colnames(DT)){
    warning('`newtime` column name found in input DT, it will be removed
            before determining new time groups')
    DT[, newtime := NULL]
  }

  if(is.null(timeThreshold)) {
    DT[, timeGroup := .GRP, by = timeField]
  } else {
    if(grepl('hour', timeThreshold)){
      if(data.table::tstrsplit(timeThreshold, ' ')[[1]] == 1){
        nTime <- 60
        newdates <- DT[, .(new =
        {new <- ifelse((data.table::minute(get(timeField)) %% nTime) > (nTime / 2),
                       (as.POSIXct(get(timeField)) +
                          (nTime - (data.table::minute(get(timeField)) %% nTime)) * 60) -
                         data.table::second(get(timeField)),
                       as.POSIXct(get(timeField)) -
                         ((data.table::minute(get(timeField)) %% (nTime)) * 60) -
                         data.table::second(get(timeField)))
        class(new) <- c("POSIXct", "POSIXct")
        new})]

        # newdates[, timeGroup := .GRP, by = new]

        # return(DT[, (colnames(newdates)) := newdates][])
      } else {
        print(data.table::as.ITime(data.table::tstrsplit(timeThreshold, ' ')[[1]],
                             format = '%H'))
        stop('this function (hourly time group) is broken')
      }

    } else if(grepl('minute', timeThreshold)){

      nTime <- unlist(data.table::tstrsplit(timeThreshold, ' ',
                                            keep = 1, type.convert = TRUE))

      # alloc.col(DT, 1 )

      newdates <- DT[, .(new =
       {new <- ifelse((data.table::minute(get(timeField)) %% nTime) > (nTime / 2),
                      (as.POSIXct(get(timeField)) +
                         (nTime - (data.table::minute(get(timeField)) %% nTime)) * 60) -
                         data.table::second(get(timeField)),
                      as.POSIXct(get(timeField)) -
                        ((data.table::minute(get(timeField)) %% (nTime)) * 60) -
                        data.table::second(get(timeField)))
       class(new) <- c("POSIXct", "POSIXct")
       new})]

      newdates[, timeGroup := .GRP, by = new]

      return(DT[, (colnames(newdates)) := newdates][])
    } else if(grepl('day', timeThreshold)){
      nTime <- unlist(data.table::tstrsplit(timeThreshold, ' ',
                                            keep = 1, type.convert = TRUE))
      if(nTime == 1){
        # DT[, timeGroup := data.table::yday(get(timeField))]
        data.table::setnames(
          data.table::data.table(DT,
                                 data.table::yday(DT[[timeField]]),
                                 data.table::yday(DT[[timeField]])),
          c(names(DT), 'day', 'timeGroup')
        )
      } else {
        days <- DT[, data.table::yday(get(timeField))]
        blockLength <- nTime
        seqBlockCuts <- seq.int(min(days), max(days) + blockLength,
                                by = blockLength)

        if(((max(days) - min(days)) / blockLength) %% 1 != 0){
          warning('the minimum and maximum days provided in DT are not evenly divisible by the block length')
        }

        data.table::setnames(
          data.table::data.table(DT,
                                 cut(days,
                                     breaks = seqBlockCuts, right = FALSE,
                                     labels = FALSE),
                                 data.table::yday(DT[[timeField]])),
          c(names(DT), 'timeGroup', 'day'))


      }
    }
  }
}

# TODO: add daily, monthly, yearly, block? times
# TODO: note results may be strange if you use something non-divisible by 60
