#' Randomizations
#'
#' @param dt input data.table with id, group fields and (optional) time fields
#' @param randomType either 'daily' or 'hourly'. 'daily' will randomize the
#'   ID for each individual 24hr trajector while 'hourly' simply randomly
#'   assigns an ID to each location
#' @param idField field indicating the id in the input data.table
#' @param groupField field indicating the group membership for each id
#' @param dateField (optional) date field used when randomType == 'daily'
#' @inheritParams BuildPts
#'
#' @export
Randomizations <- function(dt, idField, groupField, randomType, dateField = NULL) {
  if(randomType == 'hourly'){
    # TODO: this isn't really 'hourly' it's just not 'daily'
    if(!is.null(dateField)) warning('dateField ignored since randomType is hourly')

    ls.ids <- unique(dt[[idField]])

    dt[, .(randomID = sample(ls.ids, .N),
           ID = get(idField)),
       by = groupField]

  } else if(randomType == 'daily'){
    if(is.null(dateField)) stop('must provide a dateField if daily randomType chosen')
    ls.ids <- unique(dt[[idField]])
    # TODO: is it just daily or should this flex to specified time?

    # sample 1 id from the list and repeat it for the number of rows
    # so the dimensions input are the same returned
    dt[, .(randomID = rep(sample(ls.ids, 1), .N)),
       by = c(dateField, idField)]
  } else {
    stop('must provide either hourly or daily for randomType')
  }
}

# TODO: work on var names
# TODO: remove old ID once we are satisfied?
