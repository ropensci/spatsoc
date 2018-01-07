#' Randomizations
#'
#' @param DT input data.table with id, group fields and (optional) time fields
#' @param randomType one of 'daily', 'hourly' or 'spiegel'. 'daily' will randomize the ID
#'   for each individual 24hr trajector while 'hourly' simply randomly assigns
#'   an ID to each location. 'spiegel' will implement the daily movement
#'   trajectory randomizations (Spiegel et al. 2016).
#' @param idField field indicating the id in the input data.table
#' @param groupField field indicating the group membership for each id
#' @param timeField (optional) time field used for providing datetime or hour field or group time field
#' @inheritParams BuildPts
#' @param iterations The number of iterations to randomize
#'
#' @seealso
#'   \url{http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full}
#' @export
Randomizations <- function(DT, idField, groupField, randomType, dateField = NULL, iterations = NULL) {
  if(any(!(c(idField, groupField, dateField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!(randomType %in% c('hourly', 'daily', 'spiegel'))) stop('must provide either hourly, daily or spiegel for randomType')

  if(!is.numeric(iterations)) stop('must provide a numeric for iterations or NULL')
  if(is.null(iterations)) iterations <- 1

  if(iterations == 1){
    if(randomType == 'hourly'){
      # if(!is.null(dateField)) warning('dateField ignored since randomType is hourly')
      if(is.null(dateField)) stop('dateField required, please provide datetime field')
      DT[, randomID := sample(get(idField)), by = get(dateField)]

      return(DT[])
    } else if(randomType == 'daily'){
      if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
        stop('provided dateField is not of class POSIXct or IDate, for daily random type
             please provide a datetime column or IDate')
      }
      DT[, yday := data.table::yday(get(dateField))]

      dailyIDs <- DT[, .(ID = unique(ID)), by = yday(datetime)]
      dailyIDs[, randomID := sample(ID), by = yday]
      return(merge(DT, dailyIDs, on = 'yday'))

      } else if(randomType == 'spiegel'){
        if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided dateField is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        DT[, yday := data.table::yday(get(dateField))]
        idDays <- DT[, .(yday = unique(yday)), by = ID]
        idDays[, randomYday := sample(yday)]
        merged <- merge(DT, idDays, on = c('yday', 'ID'))[,
          randomDateTime := as.POSIXct(get(dateField)) + (86400 * (randomYday - yday))]
        attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  } else {
    DT[, rowID := .I]
    replicated <- DT[rep(1:.N, iterations)][, iter := 1:.N, by = rowID]

    if(randomType == 'hourly'){
      if(is.null(dateField)) stop('dateField required, please provide datetime field')
      replicated[, randomID := sample(get(idField)), by = .(iter, get(dateField))]

      return(replicated[])
    } else if(randomType == 'daily'){
      if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
        stop('provided dateField is not of class POSIXct or IDate, for daily random type
             please provide a datetime column or IDate')
      }
      replicated[, yday := data.table::yday(get(dateField))]

      dailyIDs <- replicated[, .(ID = unique(ID), datetime = get(dateField)), by = .(iter, yday(datetime))]
      dailyIDs[, randomID := sample(ID), by = .(iter, yday)]
      return(merge(replicated, dailyIDs, on = c('iter', 'yday')))

      } else if(randomType == 'spiegel'){
        if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided dateField is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        replicated[, yday := data.table::yday(get(dateField))]
        idDays <- replicated[, .(yday = unique(yday)), by = .(ID, iter)]
        idDays[, randomYday := sample(yday), by = iter]
        merged <- merge(replicated, idDays,
                        on = c('yday', 'ID', 'iter'))[, randomDateTime := as.POSIXct(get(dateField)) + (86400 * (randomYday - yday)),
                                                      by = iter][]
        attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  }
}
