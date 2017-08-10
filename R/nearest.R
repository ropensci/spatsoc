#' Proportion/Frequency Nearest Neighbors
#'
#' @inheritParams BuildPts
#' @param timeField Time field in the dt upon which the nearest neighbors will
#'   be calculate
#' @return
#' @export
#'
#' @examples
Nearest <- function(dt, timeField = NULL, proportions = FALSE, coordFields = c('EASTING', 'NORTHING'),
                    idField = 'ID'){
  if(is.null(timeField)){
    tree <- SearchTrees::createTree(dt[, ..coordFields])
    knn <- (SearchTrees::knnLookup(tree, newdat = dt[, ..coordFields], k = 2))

    data.table::data.table(ID = dt[, get(idField)],
                           neighbor = dt[, get(idField)][knn[,2]])
  } else {
    d <- dt[, {#if(.SD[, uniqueN(get(idField))] < 2){
               rowIDs <- .SD[, .(id = get(idField), .I)]
               tree <- SearchTrees::createTree(.SD[, ..coordFields])
               knn <- (SearchTrees::knnLookup(tree, newdat = .SD[, ..coordFields], k = 2))
               list(ID = rowIDs[, id],
                    neighbor = rowIDs$id[knn[,2]])
               },
       by = timeField, .SDcols = c(coordFields, idField)]
    if(!proportions){
      return(d)
    } else {
      d[, nTime := data.table::uniqueN(get(timeField)), by = ID]
      unique(d[, .(prop = .N / nTime), by = .(ID, neighbor)])
    }
  }
}

# TODO: eval(idField) or the like for naming the list with input idField
# TODO: ?? by date within by month/season/year?? for reducing proportions less than a
#       single matrix

# TODO: check that there aren't ever any ID == neighbor
#       NOTE--- this occurs because ???
# TODO: check for all unique IDs on input
