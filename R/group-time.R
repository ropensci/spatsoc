#' Group Times
#'
#' Assign a numerical group for rows by provided time column. If
#' using GPS collar data or other data with variability in concurrent measures across individuals,
#' it is recommended to round this time column providing the timeThreshold.
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
  if (!truelength(DT))
    setDT(DT)

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
      data.table::as.ITime(data.table::tstrsplit(timeThreshold, ' ',
                                                 type.convert = TRUE, keep = 1),
                           format = '%H')

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
        DT[, timeGroup := data.table::yday(get(timeField))]
      } else {
        days <- DT[, data.table::yday(get(timeField))]
        blockLength <- nTime
        seqBlockCuts <- seq.int(min(days), max(days) + blockLength,
                                by = blockLength)

        DT[, timeGroup := cut(data.table::yday(get(timeField)),
                              breaks = seqBlockCuts, right = FALSE,
                              labels = FALSE)]

        if(((max(days) - min(days)) / blockLength) %% 1 != 0){
          warning('the minimum and maximum days provided in DT are not
          evenly divisible by the block length')
        }
      }
    }
  }
}

# TODO: add daily, monthly, yearly, block? times
# TODO: note results may be strange if you use something non-divisible by 60
