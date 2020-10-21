#' Data-stream randomizations
#'
#' \code{randomizations} performs data-stream social network randomization. The
#' function accepts a \code{data.table} with relocation data, individual
#' identifiers and a randomization \code{type}. The \code{data.table} is
#' randomized either using \code{step} or \code{daily} between-individual
#' methods, or within-individual daily \code{trajectory} method described by
#' Spiegel et al. (2016).
#'
#' The \code{DT} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}}.
#'
#' Three randomization \code{type}s are provided: \enumerate{ \item step -
#' randomizes identities of relocations between individuals within each time
#' step. \item daily - randomizes identities of relocations between individuals
#' within each day. \item trajectory - randomizes daily trajectories within
#' individuals (Spiegel et al. 2016). }
#'
#' Depending on the \code{type}, the \code{datetime} must be a certain format:
#'
#' \itemize{ \item step - datetime is integer group created by
#' \code{group_times} \item daily - datetime is \code{POSIXct} format \item
#' trajectory - datetime is \code{POSIXct} format }
#'
#' The \code{id}, \code{datetime},  (and optional \code{splitBy}) arguments
#' expect the names of respective columns in \code{DT} which correspond to the
#' individual identifier, date time, and additional grouping columns. The
#' \code{coords} argument is only required when the \code{type} is "trajectory",
#' since the coordinates are required for recalculating spatial groups with
#' \code{group_pts}, \code{group_lines} or \code{group_polys}.
#'
#' Please note that if the data extends over multiple years, a column indicating
#' the year should be provided to the \code{splitBy} argument. This will ensure
#' randomizations only occur within each year.
#'
#' The \code{group} argument is expected only when \code{type} is 'step' or
#' 'daily'.
#'
#' For example, using \code{\link[data.table:IDateTime]{data.table::year}}:
#'
#' \preformatted{ DT[, yr := year(datetime)] randomizations(DT, type = 'step',
#' id = 'ID', datetime = 'timegroup', splitBy = 'yr') }
#'
#' \code{iterations} is set to 1 if not provided. Take caution with a large
#' value for \code{iterations} with large input \code{DT}.
#'
#' @return \code{randomizations} returns the random date time or random id along
#'   with the original \code{DT}, depending on the randomization \code{type}.
#'   The length of the returned \code{data.table} is the original number of rows
#'   multiplied by the number of iterations + 1. For example, 3 iterations will
#'   return 4x - one observed and three randomized.
#'
#'   Two columns are always returned: \itemize{ \item observed - if the rows
#'   represent the observed (TRUE/FALSE) \item iteration - iteration of rows
#'   (where 0 is the observed) }
#'
#'   In addition, depending on the randomization type, random ID or random date
#'   time columns are returned:
#'
#'   \itemize{ \item step - \code{randomID} each time step \item daily -
#'   \code{randomID} for each day and \code{jul} indicating julian day \item
#'   trajectory - a random date time ("random" prefixed to \code{datetime}
#'   argument), observed \code{jul} and \code{randomJul} indicating the random
#'   day relocations are swapped to. }
#'
#'
#' @param type one of 'daily', 'step' or 'trajectory' - see details
#' @param datetime field used for providing date time or time group - see
#'   details
#' @param group generated from spatial grouping functions - see details
#' @inheritParams group_pts
#' @param splitBy List of fields in DT to split the randomization process by
#' @param iterations The number of iterations to randomize
#'
#' @references
#' \url{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12553}
#' @export
#'
#' @family Social network tools
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Date time columns
#' DT[, datetime := as.POSIXct(datetime)]
#' DT[, yr := year(datetime)]
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '5 minutes')
#'
#' # Spatial grouping with timegroup
#' group_pts(DT, threshold = 5, id = 'ID', coords = c('X', 'Y'), timegroup = 'timegroup')
#'
#' # Randomization: step
#' randStep <- randomizations(
#'     DT,
#'     type = 'step',
#'     id = 'ID',
#'     group = 'group',
#'     datetime = 'timegroup',
#'     splitBy = 'yr',
#'     iterations = 2
#' )
#'
#' # Randomization: daily
#' randDaily <- randomizations(
#'     DT,
#'     type = 'daily',
#'     id = 'ID',
#'     group = 'group',
#'     datetime = 'datetime',
#'     splitBy = 'yr',
#'     iterations = 2
#' )
#'
#' # Randomization: trajectory
#' randTraj <- randomizations(
#'     DT,
#'     type = 'trajectory',
#'     id = 'ID',
#'     group = NULL,
#'     coords = c('X', 'Y'),
#'     datetime = 'datetime',
#'     splitBy = 'yr',
#'     iterations = 2
#' )
#'
randomizations <- function(DT = NULL,
                           type = NULL,
                           id = NULL,
                           group = NULL,
                           coords = NULL,
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
      any(class(DT[[datetime]]) %in% c('POSIXct', 'POSIXt'))) {
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
          x = 'datetime provided is POSIXct,
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
          x = 'datetime must be POSIXct for daily and trajectory randomization'
        )
      )
    }
  }
  if (type %in% c('step', 'daily')) {
    if (is.null(group)) {
      stop('group field must be provided if type is "step" or "daily"')
    }
    selCols <- c(splitBy, id, datetime, group)
  } else if (type == 'trajectory') {
    if (is.null(coords)) {
      stop('coords must be provided if type is "trajectory"')
    }
    selCols <- c(splitBy, id, coords, datetime)
  }

  repDT <- DT[, .SD, .SDcols = selCols][rep(1:.N, iterations + 1)]
  repDT[, rowID := .GRP, by = selCols]

  if (any(repDT[, .N , by = rowID]$N != iterations + 1)) {
    warning('found non-unique rows of id, datetime (and splitBy)')
  }

  repDT[, iteration := seq(0, .N - 1, 1), by = rowID]
  repDT[iteration == 0, observed := TRUE]
  repDT[iteration != 0, observed := FALSE]

  set(repDT, j = 'rowID', value = NULL)

  if (type == 'step') {
    if (is.null(splitBy)) {
      splitBy <- c(datetime, 'iteration')
    } else {
      splitBy <- c(datetime, 'iteration', splitBy)
    }

    repDT[!(observed), randomID := .SD[sample(.N, size = .N)],
          by = splitBy, .SDcols = id]

    repDT[(observed), randomID := .SD, .SDcols = id]

    return(repDT)
  }

  repDT[, jul := data.table::yday(.SD[[1]]), .SDcols = datetime]
  idDays <- unique(
    repDT[, .SD, .SDcols = c(splitBy, id, 'jul', 'iteration', 'observed')]
  )

  if (type == 'daily') {
    if (is.null(splitBy)) {
      splitBy <- c('jul', 'iteration')
    } else {
      splitBy <- c('jul', 'iteration', splitBy)
    }


    idDays[, randomID := .SD[sample(.N, size = .N)], by = splitBy, .SDcols = id]
    idDays[(observed), randomID := .SD[[1]], .SDcols = id]

    return(merge(repDT, idDays, on = splitBy, all = TRUE))

  } else if (type == 'trajectory') {
    if (is.null(splitBy)) {
      splitBy <- c(id, 'iteration')
    } else {
      splitBy <- c(id, 'iteration', splitBy)
    }

    idDays[, randomJul := sample(jul, size = .N), by = splitBy]

    merged <- merge(
      x = repDT,
      y = idDays,
      on = c('jul', splitBy),
      all = TRUE
    )

    randomDateCol <- paste0('random', datetime)
    merged[, (randomDateCol) := as.POSIXct(.SD[[1]] + (86400 * (randomJul - jul))),
           .SDcols = datetime]

    merged[(observed),
           c(randomDateCol, 'randomJul') := .SD,
           .SDcols = c(datetime, 'jul')]

    return(merged)
  }
}
