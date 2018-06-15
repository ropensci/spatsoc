#' Randomizations
#'
#' Randomization
#'
#' Randomization types:
#'
#' 'hourly' simply randomly assigns an ID to each location.
#'
#' 'daily' will randomize the ID for each individual 24hr trajectory
#'
#' 'spiegel' will implement the daily movement trajectory randomizations (Spiegel et al. 2016).
#'
#' @param DT input data.table with id, group fields and (optional) time fields
#' @param type one of 'daily', 'hourly' or 'spiegel' - see details
#' @param idField field indicating the id in the input data.table
#' @param groupField field indicating the group membership for each id
#' @param timeField (optional) time field used for providing datetime or hour field or group time field
#' @inheritParams BuildPts
#' @param splitBy List of fields in DT to split the randomization process by
#' @param iterations The number of iterations to randomize
#'

#'
#' @seealso
#'   <http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full>
#' @export
Randomizations <- function(DT = NULL,
                           type = NULL,
                           idField = NULL,
                           dateField = NULL,
                           splitBy = NULL,
                           iterations = NULL
) {
  if(any(!(c(idField, groupField, dateField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }

  if(!(type %in% c('hourly', 'daily', 'spiegel'))) stop('must provide either hourly, daily or spiegel for type')

  if(!is.numeric(iterations) & !is.null(iterations)) stop('must provide a numeric for iterations or NULL')

  if(is.null(iterations)) iterations <- 1
# warning

  if(iterations == 1){
    if(type == 'hourly'){
      if(is.null(dateField)) stop('dateField required, please provide datetime field')
      if(is.null(splitBy)) groupFields <- dateField else groupFields <- c(splitBy, dateField)
      DT[, randomID := sample(get(idField)), by = groupFields]

      return(DT[])
    } else if(type == 'daily'){
      if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
        stop('provided dateField is not of class POSIXct or IDate, for daily random type
             please provide a datetime column or IDate')
      }
      DT[, yday := data.table::yday(get(dateField)), by = splitBy]
      # is this dangerous if the splitBy is NULL?
      dailyIDs <- DT[, .(ID = unique(ID)), by = c(splitBy, 'yday')]
      dailyIDs[, randomID := sample(ID), by = c(splitBy, 'yday')]
      return(merge(DT, dailyIDs, on = c('yday', splitBy)))

      } else if(type == 'spiegel'){
        if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided dateField is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        DT[, yday := data.table::yday(get(dateField))]
        idDays <- DT[, .(yday = unique(yday)), by = c(idField, splitBy)]
        idDays[, randomYday := sample(yday), by = c(idField, splitBy)]
        merged <- merge(DT, idDays, on = c('yday', idField, splitBy))[,
          randomDateTime := as.POSIXct(get(dateField)) + (86400 * (randomYday - yday))]
        attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  } else {
    DT[, rowID := .I]
    replicated <- DT[rep(1:.N, iterations + 1)][, iter := seq(0, .N-1, 1), by = rowID]
    replicated[iter == 0, observed := 1]
    replicated[iter != 0, observed := 0]
    if(type == 'hourly'){
      if(is.null(dateField)) stop('dateField required, please provide datetime field')
      replicated[observed != 1, randomID := sample(get(idField)), by = c('iter', dateField, splitBy)]
      replicated[observed == 1, randomID := get(idField)]
      return(replicated[])
      } else if(type == 'daily'){

        if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided dateField is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        replicated[, yday := data.table::yday(get(dateField))]

        dailyIDs <- unique(replicated[, .(ID = get(idField), observed),
                               by = .(iter, yday, splitBy)])
        dailyIDs[, randomID := sample(ID), by = .(iter, yday, splitBy)]
        dailyIDs[observed == 1, randomID := ID]
        return(merge(replicated, dailyIDs, on = c('iter', 'yday', splitBy), all = TRUE))

      } else if(type == 'spiegel'){
        if(length(intersect(class(DT[[dateField]]), c('POSIXct', 'POSIXt', 'IDate', 'Date'))) == 0){
          stop('provided dateField is not of class POSIXct or IDate, for daily random type
               please provide a datetime column or IDate')
        }
        replicated[, yday := data.table::yday(get(dateField))]
        idDays <- replicated[, .(yday = unique(yday)), by = c(idField, 'iter', splitBy)]
        idDays[, randomYday := sample(yday), by = c(idField, 'iter', splitBy)]
        merged <- merge(replicated, idDays,
                        on = c('yday', idField, 'iter', splitBy),
                        all = TRUE)[, randomDateTime := as.POSIXct(get(dateField)) + (86400 * (randomYday - yday))]
        merged[observed == 1, c('randomDateTime', 'randomYday') := .(get(dateField), yday(get(dateField)))]
        # this is needed until data.table 1.10.5 is released.. otherwise rm it
        # attr(merged$randomDateTime, 'tzone') <- ""
        return(merged)
      }
  }
}
