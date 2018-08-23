#' Randomizations
#'
#' \code{randomizations} performs data-stream social network randomization. The function accepts a \code{data.table} with relocation data, individual identifiers and a randomization \code{type}. The \code{data.table} is randomized either using \code{step} or \code{daily} between-individual methods, or within-individual daily \code{trajectory} method described by Spiegel et al. (2016).
#'
#' The \code{DT} must be a \code{data.table}. If your data is a \code{data.frame}, you can convert it by reference using \code{\link[data.table:setDT]{data.table::setDT}}.
#'
#' Three randomization \code{type}s are provided:
#' \enumerate{
#'   \item step - randomizes identities of relocations between individuals within each time step.
#'   \item daily - randomizes identities of relocations between individuals within each day.
#'   \item trajectory - randomizes daily trajectories within individuals (Spiegel et al. 2016).
#' }
#'
#' Depending on the \code{type}, the \code{datetime} must be a certain format:
#'
#' \itemize{
#'   \item step - datetime is integer group created by \code{group_times}
#'   \item daily - datetime is \code{POSIXct} format
#'   \item trajectory - datetime is \code{POSIXct} format
#' }
#'
#' The \code{id}, \code{datetime},  (and optional \code{splitBy}) arguments expect the names of respective columns in \code{DT} which correspond to the individual identifier, date time, and additional grouping columns.
#'
#' The \code{iterations} is set to 1 if not provided. Take caution with a large value for \code{iterations} with large input \code{DT}.
#'
#' @param type one of 'daily', 'step' or 'trajectory' - see details
#' @param datetime field used for providing date time or time group - see details
#' @inheritParams group_pts
#' @param splitBy List of fields in DT to split the randomization process by
#' @param iterations The number of iterations to randomize
#'
#' @references
#'   \url{http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full}
#' @export
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '5 minutes')
#'
#' # Spatial grouping without timegroup
#' group_pts(DT, threshold = 5, id = 'ID', coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Randomization: step
#' randomizations(DT, type = 'step', id = 'ID', datetime = 'timegroup')
#'
#' # Randomization: daily
#' randomizations(DT, type = 'daily', id = 'ID', datetime = 'datetime')
#'
#' # Randomization: trajectory
#' randomizations(DT, type = 'trajectory', id = 'ID', datetime = 'datetime')
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
