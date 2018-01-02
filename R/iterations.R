#' Iterations
#'
#' @param iterations The number of iterations to randomize
#' @inheritParams Randomizations
#' @inheritParams BuildPts
#'
#' @seealso
#'   \url{http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12553/full}
#' @export
Iterations <- function(DT, idField, iterations, groupField, randomType, dateField) {
  if(any(!(c(idField, groupField, dateField) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(!is.numeric(iterations)){
    warning('number of iterations must be numeric')
    iterations <- as.numeric(iterations)
  }
  # if length of rowID
  # if type random not one of 3
  # if missing args..

  # Add rowID col
  DT[, rowID := .I]

  # This provides us with a preallocated space for filling in the randomIDs
  replicated <- DT[rep(1:.N, iterations)][, iteration := 1:.N, by = rowID]

  #generate randomIDs for each iteration (by group)
  # replicated[, randomID := spatsoc::Randomizations(.SD, idField, groupField, randomType, dateField)[,2],
  #               by = iteration]
  return(replicated[,   .(get(idField), sample(get(idField))), by = .(iteration, get(dateField))])
  # return(replicated)
}

# Iterations <- function(DT, idField, groupField, randomType, dateField = NULL) {
#   if(randomType == 'hourly'){
#     Randomizations()
#
#   } else if(randomType == 'daily'){
#     Randomizations()
#
#     } else if(randomType == 'spiegel'){
#           Randomizations()
#
#     } else {
#         stop('must provide either hourly, daily or speigel for randomType')
#       }
# }
