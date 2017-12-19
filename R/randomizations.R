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
#'
#' @seealso
#'   \url{http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full}
#' @export
Randomizations <- function(DT, idField, groupField, randomType, dateField = NULL) {
  if(any(!(c(idField, groupField, dateField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(randomType == 'hourly'){
    # if(!is.null(dateField)) warning('dateField ignored since randomType is hourly')
    #
    # lsIDs <- unique(DT[[idField]])
    # randIDs <- sample(lsIDs)
    #
    # DT[, randomID := randIDs[.GRP], by = idField]

    DT[, randomID := sample(get(idField)), by = get(dateField)]

    return(DT[])
  } else if(randomType == 'daily'){
    if(is.null(dateField)) stop('must provide a dateField if daily randomType chosen')
    listIDs <- unique(DT[[idField]])
    # TODO: is it just daily or should this flex to specified time?

    # sample 1 id from the list and repeat it for the number of rows
    # so the dimensions input are the same returned
    DT[, .(randomID = rep(sample(listIDs, 1), .N), group = get(groupField)),
       by = c(dateField, idField)]
  } else if(randomType == 'spiegel'){
    randomDatesDT <- DT[, {d <- data.table(dates =  unique(get(dateField)))
                           d[, randomN :=  sample(1:length(dates), length(dates))]
                           .SD[, .(randomDate = rep(d[randomN == .GRP, dates], .N),
                                   group = get(groupField)),
                               by = dateField]
                           },
                        by = idField]
    # DT[randomDatesDT, on = c(idField, dateField)]

  } else {
    stop('must provide either hourly or daily for randomType')
  }
}

# TODO: work on var names
# TODO: remove old ID once we are satisfied?
# TODO: change 'randomDatesDT'
# TODO: optional N random iterations?
# TODO: optionally return the original ID for checking?
