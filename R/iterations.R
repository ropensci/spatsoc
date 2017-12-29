#' Iterations
#'
#' @param iterations The number of iterations to randomize
#' @inheritParams Randomizations
#' @inheritParams BuildPts
#'
#' @seealso
#'   \url{http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full}
#' @export
Iterations <- function(DT, idField, iterations, groupField, randomType, dateField = NULL) {
  # Add rowID col
  DT[, rowID := .I]

  # Replicate the data.table (17mil rows with 100 iterations)
  # This provides us with a preallocated space for filling in the randomIDs
  replicated <- DT[rep(1:.N, iterations)][, iteration := 1:.N, by = rowID]

  #~ 2.5 minutes to generate randomIDs for each iteration (by group)
  dt.replicated[, randomID := spatsoc::Randomizations(.SD, 'ID', 'group', 'hourly')[,2],
                by = .(season, HERD, Year, iteration)]

}




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
Iterations <- function(DT, idField, groupField, randomType, dateField = NULL) {
  if(any(!(c(idField, groupField, dateField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }



  if(randomType == 'hourly'){
    Randomizations()

  } else if(randomType == 'daily'){
    Randomizations()

    } else if(randomType == 'spiegel'){
          Randomizations()

    } else {
        stop('must provide either hourly, daily or speigel for randomType')
      }
}
