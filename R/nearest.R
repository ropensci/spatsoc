#' Proportion/Frequency Nearest Neighbors
#'
#' @inheritParams BuildPts
#' @param timeField Time field in the dt upon which the nearest neighbors will
#'   be calculate
#' @return
#' @export
#'
#' @examples
Nearest <- function(dt, timeField = NULL, coordFields = c('EASTING', 'NORTHING'),
                    idField = 'ID'){
  if(is.null(timeField)){
    rowIDs <- dt[, .(id = get(idField), .I)]
    tree <- SearchTrees::createTree(dt[, ..coordFields])
    knn <- (SearchTrees::knnLookup(tree, newdat = dt[, ..coordFields], k = 2))

    data.table::data.table(ID = rowIDs[, id],
                           neighbor = rowIDs[order(match(I, knn[, 2])), id])
  } else {
    dt[, {rowIDs <- .SD[, .(id = get(idField), .I)]
          tree <- SearchTrees::createTree(.SD[, ..coordFields])
          knn <- (SearchTrees::knnLookup(tree, newdat = .SD[, ..coordFields], k = 2))
          list(ID = rowIDs[, id],
               neighbor = rowIDs[order(match(I, knn[, 2])), id])},
       by = timeField, .SDcols = c(coordFields, idField)]
  }
}

# TODO: eval(idField) or the like for naming the list with input idField
# TODO: ?? by date within by month/season/year?? for reducing proportions less than a
#       single matrix

# TODO: check that there aren't ever any ID == neighbor
# TODO: check for all unique IDs on input

# TODO: nTime is global not relative to the nLocs for each animal
