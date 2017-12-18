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

    dtm <- DT[, data.table::IDateTime(get(timeField))]


    if(grepl('hour', timeThreshold)){
      if(data.table::tstrsplit(timeThreshold, ' ')[[1]] == 1L){
        nMins <- 60L
        dtm[data.table::minute(itime) %% nMins < (nMins / 2) ,
            minutes := nMins * (data.table::minute(itime) %/% nMins)]
        dtm[data.table::minute(itime) %% nMins >= (nMins / 2),
            minutes := nMins * ((data.table::minute(itime) %/% nMins) + 1L)]

        dtm[, timegroup := .GRP,
            by = .(minutes, data.table::hour(itime), idate)]
        return(DT[, (colnames(dtm)) := dtm][])

      } else {
        nHours <- data.table::tstrsplit(timeThreshold, ' ', type.convert = TRUE)[[1]]
        if(!is.integer(nHours)) nHours <- as.integer(nHours)

        dtm <- DT[, IDateTime(get(timeField))]
        dtm[data.table::hour(itime) %% nHours < (nHours / 2) ,
            hours := nHours * (data.table::hour(itime) %/% nHours)]
        dtm[data.table::hour(itime) %% nHours >= (nHours / 2),
            hours := nHours * ((data.table::hour(itime) %/% nHours) + 1L)]

        dtm[, timegroup := .GRP, by = .(hours, idate)]
        return(DT[, (colnames(dtm)) := dtm][])
      }

    } else if(grepl('minute', timeThreshold)){
      nMins <- data.table::tstrsplit(timeThreshold, ' ')[[1]]
      if(!is.integer(nMins)) nMins <- as.integer(nMins)

      dtm[data.table::minute(itime) %% nMins < (nMins / 2) ,
          minutes := nMins * (data.table::minute(itime) %/% nMins)]
      dtm[data.table::minute(itime) %% nMins >= (nMins / 2),
          minutes := nMins * ((data.table::minute(itime) %/% nMins) + 1L)]

      dtm[, timegroup := .GRP,
          by = .(minutes, data.table::hour(itime), idate)]
      return(DT[, (colnames(dtm)) := dtm][])

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


