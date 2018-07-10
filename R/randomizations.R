#' Randomizations
#'
#' Data stream randomization methods
#'
#' Three randomization methods are provided:
#'
#' 'step' randomly assigns an ID to each location.
#'
#' 'daily' will randomize the ID for each individual 24hr trajectory
#'
#' 'trajectory' will implement the daily movement trajectory randomizations (Spiegel et al. 2016).
#'
#' @param DT input data.table with id, group fields and (optional) time fields
#' @param type one of 'daily', 'step' or 'trajectory' - see details
#' @param id field indicating the id in the input data.table
#' @param datetime (optional) time field used for providing datetime or hour field or group time field
#' @inheritParams group_pts
#' @param splitBy List of fields in DT to split the randomization process by
#' @param iterations The number of iterations to randomize
#'
#' @references
#'   <http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full>
#' @export
randomizations <- function(DT = NULL,
                           type = NULL,
                           id = NULL,
                           datetime = NULL,
                           splitBy = NULL,
                           iterations = NULL) {
  # due to NSE notes in R CMD check
  randomID <- jul <- randomJul <- rowID <- iteration <- observed <- NULL

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(type)) {
    stop('type of randomization required')
  }

  if (!(type %in% c('step', 'daily', 'trajectory'))) {
    stop('type of randomization must be one of: step, daily or trajectory')
  }

  if (is.null(id)) {
    stop('id field required')
  }

  if (is.null(datetime)) {
    stop('datetime field required')
  }

  if (any(!(c(id, datetime, splitBy) %in% colnames(DT)))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(id, datetime),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (is.null(iterations)) {
    warning('iterations is not provided therefore iterations set to 1')
    iterations <- 1L
  }

  if (!is.numeric(iterations)) {
    stop('either provide a numeric for iterations or NULL')
  }

  if (length(datetime) == 1 &&
      any(class(DT[[datetime]]) %in% c('POSIXct', 'POSIXt', 'IDate'))) {
    dateFormatted <- TRUE
  } else {
    dateFormatted <- FALSE
  }

  if (type == 'step') {
    if (dateFormatted) {
      warning(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'datetime provided is either POSIXct or IDate,
          step randomization will only be
          performed within each datetime -
          consider using group_times first and providing timegroup'
        )
      )
    }
  } else if (type == 'daily' || type == 'trajectory') {
    if (!dateFormatted) {
      stop(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'datetime must be either
          POSIXct or IDate for daily and trajectory randomization'
        )
      )
    }
    if ('jul' %in% colnames(DT)) {
      warning('column "jul" found in DT, will be overwritten by this function')
    }
  }

  if (iterations == 1) {
    if (type == 'step') {
      if (is.null(splitBy)) {
        splitBy <- datetime
      } else {
        splitBy <- c(datetime, splitBy)
      }

      DT[, randomID := .SD[sample(.N)], by = splitBy, .SDcols = id]

      return(DT[])

    }

    DT[, jul := data.table::yday(.SD[[1]]), .SDcols = datetime]
    idDays <- unique(DT[, .SD, .SDcols = c(splitBy, id, 'jul')])
    setnames(idDays, c(splitBy, id, 'jul'))

    if (type == 'daily') {
      idDays[, randomID := .SD[sample(.N)],
             by = c(splitBy, 'jul'), .SDcols = id]

      return(merge(DT, idDays, on = c(splitBy, 'jul'))[])

    } else if (type == 'trajectory') {
      idDays[, randomJul := sample(jul), by = c(id, splitBy)]
      merged <- merge(DT, idDays, on = c('jul', id, splitBy))

      randomDateCol <- paste0('random', datetime)
      merged[, (randomDateCol) :=
               as.POSIXct(.SD[[1]] + (86400 * (randomJul - jul))),
             .SDcols = datetime]
      return(merged[])
    }
  } else {
    if ('rowID' %in% colnames(DT)) {
      warning(
        strwrap(
          prefix = " ",
          initial = "",
          x = 'column "rowID" found in DT
          and will be overwritten by this function'
        )
      )
    }

    DT[, rowID := .I]
    repDT <- DT[rep(1:.N, iterations + 1)]
    repDT[, iteration := seq(0, .N - 1, 1), by = rowID]
    repDT[iteration == 0, observed := 1]
    repDT[iteration != 0, observed := 0]

    if (type == 'step') {
      repDT[observed != 1, randomID := .SD[sample(.N)],
            by = splitBy, .SDcols = id]
      repDT[observed == 1, randomID := .SD, .SDcols = id]
      return(repDT[])
    }

    repDT[, jul := data.table::yday(.SD[[1]]), .SDcols = datetime]
    idDays <- unique(repDT[, .SD,
                           .SDcols = c(splitBy, id, 'jul',
                                       'iteration', 'observed')])
    # setnames(idDays, c(splitBy, id, 'jul', 'iteration', 'observed'))

    if (type == 'daily') {
      idDays[, randomID := .SD[sample(.N)],
             by = c(splitBy, 'jul'), .SDcols = id]
      idDays[observed == 1, randomID := .SD[[1]], .SDcols = id]
      return(merge(repDT, idDays, on = c('iteration', 'jul', splitBy),
                   all = TRUE))

    } else if(type == 'trajectory'){
      idDays[, randomJul := sample(jul), by = c(id, splitBy, 'iteration')]
      merged <- merge(repDT, idDays,
                      on = c('jul', id, 'iteration', splitBy),
                      all = TRUE)
      randomDateCol <- paste0('random', datetime)
      merged[, (randomDateCol) :=
               as.POSIXct(.SD[[1]] + (86400 * (randomJul - jul))),
             .SDcols = datetime]
      merged[observed == 1,
             c(randomDateCol, 'randomJul') := .SD,
             .SDcols = c(datetime, 'jul')]
      return(merged[])
    }
  }
}
