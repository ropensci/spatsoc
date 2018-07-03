#' randomizations
#'
#' Randomization
#'
#' Randomization types:
#'
#' 'hourly' randomly assigns an ID to each location.
#'
#' 'daily' will randomize the ID for each individual 24hr trajectory
#'
#' 'trajectory' will implement the daily movement trajectory randomizations (Spiegel et al. 2016).
#'
#' @param DT input data.table with id, group fields and (optional) time fields
#' @param type one of 'daily', 'hourly' or 'trajectory' - see details
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

  if (is.null(id)) {
    stop('ID field required')
  }

  # match type to group (required/not), datetime (required/not) OR IS IT TIMEGROUP?

  if(any(!(c(id, datetime) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }

  if(!(type %in% c('hourly', 'daily', 'trajectory'))) stop('must provide either hourly, daily or trajectory for type')

  if(!is.numeric(iterations) & !is.null(iterations)) stop('must provide a numeric for iterations or NULL')

  if(is.null(iterations)) iterations <- 1
# warning

  if(iterations == 1){
    if(type == 'hourly'){
      if(is.null(datetime)) stop('datetime required, please provide datetime field')
      if(is.null(splitBy)) splitBy <- datetime else splitBy <- c(splitBy, datetime)
      DT[, randomID := sample(get(id)), by = splitBy]

      return(DT[])
    } else if(type == 'daily'){
      if(length(intersect(class(DT[[datetime]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
        stop('provided datetime is not of class POSIXct or IDate, for daily random type
             please provide a datetime column or IDate')
      }
      DT[, yday := data.table::yday(get(datetime)), by = splitBy]
      dailyIDs <- DT[, .(ID = unique(ID)), by = c(splitBy, 'yday')]
      dailyIDs[, randomID := sample(ID), by = c(splitBy, 'yday')]
      return(merge(DT, dailyIDs, on = c('yday', splitBy)))

      } else if(type == 'trajectory'){
        if(length(intersect(class(DT[[datetime]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided datetime is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        DT[, yday := data.table::yday(get(datetime))]
        idDays <- DT[, .(yday = unique(yday)), by = c(id, splitBy)]
        idDays[, randomYday := sample(yday), by = c(id, splitBy)]
        merged <- merge(DT, idDays, on = c('yday', id, splitBy))[,
          randomDateTime := as.POSIXct(get(datetime)) + (86400 * (randomYday - yday))]
        attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  } else {
    DT[, rowID := .I]
    replicated <- DT[rep(1:.N, iterations + 1)][, iter := seq(0, .N-1, 1), by = rowID]
    replicated[iter == 0, observed := 1]
    replicated[iter != 0, observed := 0]
    if(type == 'hourly'){
      if(is.null(datetime)) stop('datetime required, please provide datetime field')
      replicated[observed != 1, randomID := sample(get(id)), by = c('iter', datetime, splitBy)]
      replicated[observed == 1, randomID := get(id)]
      return(replicated[])
      } else if(type == 'daily'){

        if(length(intersect(class(DT[[datetime]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided datetime is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        replicated[, yday := data.table::yday(get(datetime))]

        dailyIDs <- unique(replicated[, .(ID = get(id), observed),
                               by = .(iter, yday, splitBy)])
        dailyIDs[, randomID := sample(ID), by = .(iter, yday, splitBy)]
        dailyIDs[observed == 1, randomID := ID]
        return(merge(replicated, dailyIDs, on = c('iter', 'yday', splitBy), all = TRUE))

      } else if(type == 'trajectory'){
        if(length(intersect(class(DT[[datetime]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided datetime is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        replicated[, yday := data.table::yday(get(datetime))]
        idDays <- replicated[, .(yday = unique(yday)), by = c(id, 'iter', splitBy)]
        idDays[, randomYday := sample(yday), by = c(id, 'iter', splitBy)]
        merged <- merge(replicated, idDays,
                        on = c('yday', id, 'iter', splitBy),
                        all = TRUE)[, randomDateTime := as.POSIXct(get(datetime)) + (86400 * (randomYday - yday))]
        merged[observed == 1, c('randomDateTime', 'randomYday') := .(get(datetime), yday(get(datetime)))]
        # this is needed until data.table 1.10.5 is released.. otherwise rm it
        # attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  }
}
