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
#' @seealso
#'   <http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full>
#' @export
randomizations <- function(DT = NULL,
                           type = NULL,
                           id = NULL,
                           datetime = NULL,
                           splitBy = NULL,
                           iterations = NULL
) {
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

  if (any(!(
    c(id, datetime) %in% colnames(DT)
  ))) {
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

  # if (is.null(splitBy)) {
  #   splitBy <- datetime
  # } else {
  #   splitBy <- c(datetime, splitBy)
  # }

  if (length(datetime) == 1 &&
      any(class(DT[[datetime]]) %in%
                c('POSIXct', 'POSIXt', 'IDate'))) {
    dateFormatted <- TRUE
  } else {
    dateFormatted <- FALSE
  }


  if (type == 'step') {
    if (dateFormatted) {
      warning(
        'datetime provided is either POSIXct or IDate, step randomization will only be performed within each datetime - consider using group_times first and providing timegroup'
      )
    }
  } else if (type == 'daily' || type == 'trajectory') {
    if (!dateFormatted) {
      stop(
        'datetime must be either POSIXct or IDate for daily and trajectory randomization'
      )
    }
  }


  if(iterations == 1){
    if(type == 'step') {
      if (is.null(splitBy)) {
        splitBy <- datetime
      } else {
        splitBy <- c(datetime, splitBy)
      }
      DT[, randomID := .SD[sample(.N)], by = splitBy, .SDcols = id]

      return(DT[])

    } else if(type == 'daily'){
      DT[, jul := data.table::yday(get(datetime)), by = splitBy]
      dailyIDs <- DT[, .(ID = unique(ID)), by = c(splitBy, 'jul')]
      dailyIDs[, randomID := sample(ID), by = c(splitBy, 'jul')]

      return(merge(DT, dailyIDs, on = c('jul', splitBy)))

      } else if(type == 'trajectory'){

        DT[, yday := data.table::yday(get(datetime))]
        idDays <- DT[, .(yday = unique(yday)), by = c(id, splitBy)]
        idDays[, randomYday := sample(yday), by = c(id, splitBy)]
        merged <- merge(DT, idDays, on = c('yday', id, splitBy))[,
          randomDateTime := as.POSIXct(get(datetime)) + (86400 * (randomYday - yday))]
        attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  } #else {
  #   DT[, rowID := .I]
  #   replicated <- DT[rep(1:.N, iterations + 1)][, iter := seq(0, .N-1, 1), by = rowID]
  #   replicated[iter == 0, observed := 1]
  #   replicated[iter != 0, observed := 0]
  #   if(type == 'step'){
  #     if(is.null(datetime)) stop('datetime required, please provide datetime field')
  #     replicated[observed != 1, randomID := sample(get(id)), by = c('iter', datetime, splitBy)]
  #     replicated[observed == 1, randomID := get(id)]
  #     return(replicated[])
  #     } else if(type == 'daily'){
  #
  #       if(length(intersect(class(DT[[datetime]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
  #         stop('provided datetime is not of class POSIXct or IDate, for daily random type
  #              please provide a datetime column or IDate')
  #       }
  #       replicated[, yday := data.table::yday(get(datetime))]
  #
  #       dailyIDs <- unique(replicated[, .(ID = get(id), observed),
  #                              by = .(iter, yday, splitBy)])
  #       dailyIDs[, randomID := sample(ID), by = .(iter, yday, splitBy)]
  #       dailyIDs[observed == 1, randomID := ID]
  #       return(merge(replicated, dailyIDs, on = c('iter', 'yday', splitBy), all = TRUE))
  #
  #     } else if(type == 'trajectory'){
  #       if(length(intersect(class(DT[[datetime]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
  #         stop('provided datetime is not of class POSIXct or IDate, for daily random type
  #              please provide a datetime column or IDate')
  #       }
  #       replicated[, yday := data.table::yday(get(datetime))]
  #       idDays <- replicated[, .(yday = unique(yday)), by = c(id, 'iter', splitBy)]
  #       idDays[, randomYday := sample(yday), by = c(id, 'iter', splitBy)]
  #       merged <- merge(replicated, idDays,
  #                       on = c('yday', id, 'iter', splitBy),
  #                       all = TRUE)[, randomDateTime := as.POSIXct(get(datetime)) + (86400 * (randomYday - yday))]
  #       merged[observed == 1, c('randomDateTime', 'randomYday') := .(get(datetime), yday(get(datetime)))]
  #       # this is needed until data.table 1.10.5 is released.. otherwise rm it
  #       # attr(merged$randomDateTime, 'tzone') <- ""
  #       return(merged)
  # }
  # }
}
