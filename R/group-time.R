#' Group Times
#'
#' Assign an integer group for rows by provided time column. If
#' using GPS collar data or other data with variability in concurrent measures
#' across individuals, it is usefuls to round this time column
#' providing the threshold.
#' Otherwise, rows are grouped by matching exact times.
#'
#' This function can also group rows on blocks of days.
#' Simply provide the block length in the same manner as the threshold eg: '5 days'.
#'
#' @param DT input locs/rows
#' @param timeField time column name
#' @param threshold character defining the threshold for grouping times.
#'                      eg: '2 hours', '10 minutes', etc.
#'                      if not provided, times will be matched exactly.
#'                      Note that provided threshold must be in the expected format: '## unit'
#'
#' @export
GroupTimes <- function(DT = NULL,
                       threshold = NULL,
                       timeField = NULL) {
  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(timeField)) {
    stop('time field required')
  }

  if (!(timeField %in% colnames(DT))) {
    stop('time field provided is not found in DT')
  }

  checkCols <- c('hours', 'minutes', 'block', 'timegroup')

  if (any(checkCols %in% colnames(DT))) {
    warning(paste0(
      paste(as.character(intersect(
        colnames(DT), checkCols
      )), collapse = ', '),
      ' columns found in input DT and will be overwritten by this function'
    ))
    set(DT, j = intersect(colnames(DT), checkCols), value = NULL)
    # DT[, (intersect(colnames(DT), checkCols)) :=  NULL]

  }

  if (is.null(threshold)) {
    warning('no threshold provided, using the time field directly to group')
    DT[, timegroup := .GRP, by = timeField]
    return(DT)
  } else {
    if ('POSIXct' %in% unlist(lapply(DT[, .(get(timeField))], class))) {
      dtm <-
        DT[, cbind(get(timeField), data.table::IDateTime(get(timeField)))]
      data.table::setnames(dtm, c(timeField, 'idate', 'itime'))
    } else if (length(timeField) == 2 &&
               all(c('IDate', 'ITime') %in%
                   unlist(lapply(DT[, .SD, .SDcols = timeField],
                                 class)))) {
      dtm <- DT[, .SD, .SDcols = timeField]
      data.table::setnames(dtm, c('idate', 'itime'))
    } else {
      stop('time field provided must be either 1 column: POSIXct or 2 columns: IDate and ITime')
    }

    if (grepl('hour', threshold) &&
        data.table::tstrsplit(threshold, ' ')[[1]] == 1L) {
      threshold <- '60 minutes'
    }

    if (grepl('hour', threshold)) {
      nHours <- data.table::tstrsplit(threshold, ' ')[[1]]
      if (!is.integer(nHours)) {
        nHours <- as.integer(nHours)
      }

      dtm[data.table::hour(itime) %% nHours < (nHours / 2) ,
          hours := nHours * (data.table::hour(itime) %/% nHours)]
      dtm[data.table::hour(itime) %% nHours >= (nHours / 2),
          hours := nHours * ((data.table::hour(itime) %/% nHours) + 1L)]

      dtm[, timegroup := .GRP, by = .(hours, idate)]
      # DT[, (colnames(dtm)) := dtm][]
      set(DT, j = colnames(dtm), value = dtm)
      return(DT)
    } else if (grepl('minute', threshold)) {
      nMins <- data.table::tstrsplit(threshold, ' ')[[1]]
      if (!is.integer(nMins)) {
        nMins <- as.integer(nMins)
      }

      if (nMins > 60) {
        stop('threshold provided with > 60 minutes')
      }
      if (60 %% nMins) {
        stop('threshold not evenly divisible by 60')
      }

      dtm[data.table::minute(itime) %% nMins < (nMins / 2) ,
          minutes := nMins * (data.table::minute(itime) %/% nMins)]
      dtm[data.table::minute(itime) %% nMins >= (nMins / 2),
          minutes := nMins * ((data.table::minute(itime) %/% nMins) + 1L)]
      dtm[, timegroup := .GRP,
          by = .(minutes, data.table::hour(itime), idate)]
      # DT[, (colnames(dtm)) := dtm][]
      set(DT, j = colnames(dtm), value = dtm)
      return(DT)
    } else if (grepl('day', threshold)) {
      nDays <- data.table::tstrsplit(threshold, ' ')[[1]]
      if (!is.integer(nDays)) {
        nDays <- as.integer(nDays)
      }
      if (nDays == 1) {
        dtm[, timegroup := .GRP,
            by = .(data.table::year(idate), data.table::yday(idate))]
        DT[, (colnames(dtm)) := dtm][]
        # set(DT, j = colnames(dtm), value = dtm)
        return(DT)
      } else {
        minday <- dtm[, min(data.table::yday(idate))]
        maxday <- dtm[, max(data.table::yday(idate))]
        if (((maxday - minday) / nDays) %% 1 != 0) {
          warning(
            'the minimum and maximum days in DT are not evenly divisible by the provided block length',
            '\n min day = ',
            as.character(minday),
            ', max day = ',
            as.character(maxday)
          )
        }
        dtm[, timegroup := cut(
          data.table::yday(idate),
          breaks = seq.int(minday, maxday + nDays, by = nDays),
          right = FALSE,
          labels = FALSE
        )]
        # return(DT[, (colnames(dtm)) := dtm][])
        set(DT, j = colnames(dtm), value = dtm)
        return(DT)
      }
    } else {
      stop("must provide threshold in units of hour, day, or minute")
    }
    }
}
