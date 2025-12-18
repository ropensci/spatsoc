#' Group times
#'
#' `group_times` groups rows into time groups. The function expects date
#' time formatted data and a threshold argument. The threshold argument is used
#' to specify a time window within which rows are grouped.
#'
#' The `DT` must be a `data.table`. If your data is a
#' `data.frame`, you can convert it by reference using
#' [data.table::setDT()].
#'
#' The `datetime` argument expects the name of a column in `DT` which
#' is of type `POSIXct` or the name of two columns in `DT` which are
#' of type `IDate` and `ITime`.
#'
#' `threshold` must be provided in units of minutes, hours or days. The
#' character string should start with an integer followed by a unit, separated
#' by a space. It is interpreted in terms of 24 hours which poses the following
#' limitations:
#'
#' \itemize{ \item minutes, hours and days cannot be fractional \item minutes
#' must divide evenly into 60 \item minutes must not exceed 60 \item minutes,
#' hours which are nearer to the next day, are grouped as such \item hours must
#' divide evenly into 24 \item multi-day blocks should divide into the range of
#' days, else the blocks may not be the same length }
#'
#' In addition, the `threshold` is considered a fixed window throughout the
#' time series and the rows are grouped to the nearest interval.
#'
#' If `threshold` is NULL, rows are grouped using the `datetime`
#' column directly.
#'
#' @return `group_times` returns the input `DT` appended with a
#'   `timegroup` column and additional temporal grouping columns to help
#'   investigate, troubleshoot and interpret the timegroup.
#'
#'   The actual value of `timegroup` is arbitrary and represents the
#'   identity of a given `timegroup` which 1 or more individuals are
#'   assigned to. If the data was reordered, the group may change, but the
#'   contents of each group would not.
#'
#'   The temporal grouping columns added depend on the `threshold`
#'   provided:
#'
#'   \itemize{ \item `threshold` with unit minutes: "minutes" column added
#'   identifying the nearest minute group for each row. \item `threshold`
#'   with unit hours: "hours" column added identifying the nearest hour group
#'   for each row. \item `threshold` with unit days: "block" columns added
#'   identifying the multiday block for each row. }
#'
#'   A message is returned when any of these columns already exist in the input
#'   `DT`, because they will be overwritten.
#'
#'   See details for appending outputs using modify-by-reference in the
#'   [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html).
#'
#'
#' @inheritParams group_pts
#' @param datetime name of date time column(s). either 1 POSIXct or 2 IDate and
#'   ITime. e.g.: 'datetime' or c('idate', 'itime')
#' @param threshold threshold for grouping times. e.g.: '2 hours', '10 minutes',
#'   etc. if not provided, times will be matched exactly. Note that provided
#'   threshold must be in the expected format: '## unit'
#'
#' @export
#'
#' @family Temporal grouping
#' @seealso `group_pts` `group_lines`
#'   `group_polys`
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' group_times(DT, datetime = 'datetime', threshold = '5 minutes')
#'
#' group_times(DT, datetime = 'datetime', threshold = '2 hours')
#'
#' group_times(DT, datetime = 'datetime', threshold = '10 days')
#'
group_times <- function(
    DT = NULL,
    datetime = NULL,
    threshold = NULL) {
  # due to NSE notes in R CMD check
  minutes <- block <- hours <- itime <- . <- idate <- timegroup <- NULL
  minday <- maxday <- rangeday <- adjIDate <- adjHour <-  NULL

  assert_not_null(DT)
  assert_is_data_table(DT)
  assert_not_null(datetime)
  assert_are_colnames(DT, datetime)

  checkCols <- c('hours', 'minutes', 'block', 'timegroup')

  if (any(checkCols %in% colnames(DT))) {
    message(paste0(
      paste(as.character(intersect(
        colnames(DT), checkCols
      )), collapse = ', '),
      ' columns found in input DT and will be overwritten by this function'
    ))
    data.table::set(DT, j = intersect(colnames(DT), checkCols), value = NULL)
  }

  if (is.null(threshold)) {
    message('no threshold provided, using the time field directly to group')
    DT[, timegroup := .GRP, by = c(datetime)]
    return(DT[])
  } else {
    if (length(datetime) == 1 &&
        'POSIXct' %in% unlist(lapply(DT[, .SD, .SDcols = datetime], class))) {
      dtm <-
        DT[, cbind(.SD[[1]], data.table::IDateTime(.SD[[1]])),
           .SDcols = datetime]
      data.table::setnames(dtm, c(datetime, 'idate', 'itime'))
    } else if (length(datetime) == 2 &&
               all(c('IDate', 'ITime') %in%
                   unlist(
                     lapply(DT[, .SD, .SDcols = datetime], class)))) {
      dtm <- DT[, .SD, .SDcols = datetime]
      data.table::setnames(dtm, c('idate', 'itime'))
    } else {
      stop(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'time field provided must be
          either 1 column: POSIXct or 2 columns: IDate and ITime'
        )
      )
    }

    if (grepl('hour', threshold) &&
        data.table::tstrsplit(threshold, ' ')[[1]] == 1L) {
      threshold <- '60 minutes'
    }

    if (grepl('hour', threshold)) {
      nHours <- data.table::tstrsplit(threshold, ' ',
                                      type.convert = TRUE)[[1]]
      if (!is.integer(nHours)) {
        if (nHours %% 1 != 0) {
          warning(
            strwrap(
              prefix = " ",
              initial = "",
              x = 'number of hours provided
              cannot be a fractional - threshold will be rounded'
            )
          )
        }
        nHours <- as.integer(nHours)
      }

      if (24 %% nHours != 0) {
        stop(
          strwrap(
            prefix = " ",
            initial = "",
            x = 'number of hours provided
                 does not evenly divide into 24'
          )
        )
      }

      dtm[data.table::hour(itime) %% nHours < (nHours / 2),
          hours := nHours * (data.table::hour(itime) %/% nHours)]
      dtm[data.table::hour(itime) %% nHours >= (nHours / 2),
          hours := nHours * ((data.table::hour(itime) %/% nHours) + 1L)]

      dtm[, adjIDate := idate]
      dtm[hours == 24, c('adjIDate', 'hours') := .(idate + 1, 0)]

      dtm[, timegroup := .GRP, by = .(hours, adjIDate)]
      data.table::set(dtm, j = 'adjIDate', value = NULL)
      data.table::set(dtm, j = c('idate', 'itime'), value = NULL)
      DT[, (colnames(dtm)) := dtm]

      return(DT[])
    } else if (grepl('minute', threshold)) {
      nMins <- data.table::tstrsplit(threshold, ' ',
                                     type.convert = TRUE)[[1]]
      if (!is.integer(nMins)) {
        if (nMins %% 1 != 0) {
          warning(
            strwrap(
              prefix = " ",
              initial = "",
              x = 'number of minutes provided
              cannot be a fractional - threshold will be rounded'
            )
          )
        }
        nMins <- as.integer(nMins)
      }

      assert_relation(nMins, `<=`, 60, ... = ' minutes')

      if (60 %% nMins != 0) {
        stop('threshold not evenly divisible by 60')
      }

      dtm[data.table::minute(itime) %% nMins < (nMins / 2) ,
          minutes := nMins * (data.table::minute(itime) %/% nMins)]
      dtm[data.table::minute(itime) %% nMins >= (nMins / 2),
          minutes := nMins * ((data.table::minute(itime) %/% nMins) + 1L)]

      dtm[, c('adjMinute', 'adjHour', 'adjDate') :=
            .(minutes, data.table::hour(itime), idate)]

      dtm[minutes == 60L,
          c('adjMinute', 'adjHour') :=
            .(0L, adjHour + 1L)]

      dtm[adjHour == 24L,
      c('adjMinute', 'adjHour', 'adjDate') :=
        .(0L, 0L, idate + 1)]

      dtm[, timegroup := .GRP,
          by = c('adjMinute', 'adjHour', 'adjDate')]

      data.table::set(dtm, j = c('adjMinute', 'adjHour', 'adjDate'), value = NULL)
      data.table::set(dtm, j = c('idate', 'itime'), value = NULL)

      DT[, (colnames(dtm)) := dtm]
      return(DT[])
    } else if (grepl('day', threshold)) {
      nDays <- data.table::tstrsplit(threshold, ' ')[[1]]
      if (!is.integer(nDays)) {
        nDays <- as.integer(nDays)
      }
      if (nDays == 1) {
        dtm[, timegroup := .GRP,
            by = .(data.table::year(idate), data.table::yday(idate))]

        data.table::set(dtm, j = c('idate', 'itime'), value = NULL)
        DT[, (colnames(dtm)) := dtm]
        return(DT[])
      } else {

        minday <- dtm[, min(data.table::yday(idate))]
        maxday <- dtm[, max(data.table::yday(idate))]
        rangeday <- dtm[, maxday - minday]

        if (!(rangeday %% nDays == 0)) {
          warning(
            strwrap(
              prefix = " ",
              initial = "",
              x = paste0('the minimum and maximum days in
              DT are not evenly divisible by the provided block length'
              ))
          )
        }

        dtm[, block := cut(
          data.table::yday(idate),
          breaks = seq.int(minday, maxday + nDays, by = nDays),
          right = FALSE,
          labels = FALSE
        )]

        dtm[, timegroup := .GRP, .(year(idate), block)]
        data.table::set(dtm, j = c('idate', 'itime'),
            value = NULL)
        DT[, (colnames(dtm)) := dtm]
        return(DT[])
      }
    } else {
      stop("must provide threshold in units of hour, day, or minute")
    }
  }
}
